;;; jane-aide.el --- AIDE support                    -*- lexical-binding: t; -*-

;;; Commentary:

;; This package provides functions which can be added to `completion-at-point-functions'
;; to make AI-based completions (drawn from AIDE) available through `completion-at-point'
;; or other completion UIs.
;;
;; The main entry point is `jane-aide-completion-at-point'; this returns AI completions,
;; as you'd expect.  It maintains per-buffer state in `jane-aide--last-requested' and
;; `jane-aide--last-fetched' to avoid unnecessary completion requests.
;;
;; The other main feature is single-symbol completion; the main entry point for that is
;; `jane-aide-add-ai-symbols-to-capf'.

;;; Code:

(require 'project)
(declare-function jane-ecaml-version "ecaml_plugin" (&optional interactive) t)
(declare-function jane-aide--start "ecaml_plugin" (version logdir mode) t)
(declare-function jane-aide--stop "ecaml_plugin" (client) t)
(declare-function jane-aide--request-finished-p "ecaml_plugin" (request) t)
(declare-function jane-aide--request-wait "ecaml_plugin" (request) t)
(declare-function jane-aide--request-start "ecaml_plugin"
                  (client workspace file language coding-system-eol-type text offset)
                  t)
(declare-function jane-aide--log-completion-accepted "ecaml_plugin" (client string) t)

(defvar jane-aide--client nil
  "The global shared AIDE client, used for all AIDE completion.")

(defcustom jane-aide-mode 'prod
  "The mode that should be used when creating an AIDE client.

Must be either `prod' or `dev'. Changing this will not take
effect without restarting the AIDE client. You can restart the
AIDE client with \\[jane-aide-client-restart]."
  :type '(choice (const :tag "Prod" prod)
                 (const :tag "Dev" dev))
  :group 'jane-aide)

(defun jane-aide--client-get ()
  "Get `jane-aide--client', creating it if necessary."
  (unless jane-aide--client
    (let ((dir (expand-file-name "aide" user-emacs-directory)))
      (make-directory dir t)
      (setq jane-aide--client
            (jane-aide--start
             (format "%s %s" (emacs-version) (jane-ecaml-version)) dir jane-aide-mode))))
  jane-aide--client)

(defun jane-aide--client-stop ()
  "Stop `jane-aide--client', if non-nil."
  (when jane-aide--client
    (let ((old jane-aide--client))
      (setq jane-aide--client nil)
      (jane-aide--stop old))))

(defun jane-aide-client-restart ()
  "Restart `jane-aide--client'."
  (interactive)
  (jane-aide--client-stop)
  (jane-aide--client-get))


(defun jane-aide--completion-table-make ()
  "Request AI completions, and return a completion table for them.

We return a programmed completion table (see Info
node `(elisp)Programmed Completion'), which is just a lambda
wrapping the completions.  This gives us a few properties:

- This function just requests the completions, it doesn't
  actually wait for them to arrive.
- We only wait for the completions when the completion table
  actually gets invoked.
- Whether the completions have arrived can be checked by calling
  the completion table with the symbol `jane-aide-fetched-p' as
  action.

Also, the table lists `aide' as its completion category; see
`completion-category-defaults'.

Also, each returned completion has a `jane-aide-completion' text
property, used to distinguish it

This function is the only thing that would need to be replaced to
switch to a different backend."
  (let ((request
         (jane-aide--request-start
          (jane-aide--client-get)
          (when-let ((proj (project-current)))
            ;; project-root may return something like ~/src, which Emacs considers
            ;; to be an absolute path, but File_path.Absolute doesn't.
            (expand-file-name (project-root proj)))
          buffer-file-name
          (cond
           ((derived-mode-p 'lisp-data-mode) "lisp")
           ((derived-mode-p 'tuareg-mode) "ocaml")
           ;; If AIDE someday wants to know the specific shell, we can check sh-shell.
           ((derived-mode-p 'sh-mode) "shell")
           ((derived-mode-p 'python-mode) "python")
           ((derived-mode-p 'fsharp-mode) "fsharp")
           ((derived-mode-p 'c-mode) "c")
           ;; Detect things like M-: or ielm which don't use lisp-data-mode but still are Lisp
           ((memq 'elisp-completion-at-point completion-at-point-functions) "lisp")
           ;; Fall back on inferring the filetype from the filename, if there is one.
           (t nil))
          (let ((eol-type (coding-system-eol-type buffer-file-coding-system)))
            (if (vectorp eol-type) 0 eol-type))
          (buffer-substring-no-properties (point-min) (point-max))
          ;; Pass the byte offset of (point) in the buffer text.
          (- (position-bytes (point)) (position-bytes (point-min))))))
    (lambda (string pred action)
      (cl-case action
        ;; CR-someday sbaugh: In Emacs 30 the category can be set by
        ;; `jane-aide-completion-at-point'; we should move it there, which will make it
        ;; easier to replace `jane-aide--completion-table-make'.
        (metadata '(metadata (category . aide)))
        (jane-aide-fetched-p (jane-aide--request-finished-p request))
        (t
         ;; CR-someday sbaugh: The request should raise an error if it was cancelled; we should
         ;; detect cancellation and turn it into a nil instead.  Or maybe re-request?  Either
         ;; way, hard to test since we can't actually get a cancellation right now.
         (complete-with-action action (jane-aide--request-wait request) string pred))))))

;; CR-someday sbaugh: We should add a new "aide" completion style and configure it here.
;; That would let us do two things:
;; - move point based on the new value of point embedded in each completion
;; - use the virtual text of each completion to check if the current buffer text can
;; expand to that completion (instead of just checking that the current buffer text is a
;; prefix of a completion); this would cause the last-fetched completions to be used in
;; more cases.
(add-to-list 'completion-category-defaults
             '(aide . ((styles . (basic substring)))))

(defcustom jane-aide-fetch-idly nil
  "Whether to fetch AI completions in idle completion."
  :type '(choice (const :tag "Fetch when in idle completion." t)
                 (const :tag "Only fetch when explicitly requested" nil))
  :group 'jane-completion)

(defun jane-aide--completion-table-fetched-p (table)
  "Does TABLE actually have completions yet?

If it doesn't, then any use of this table will first wait for the
completions to be fetched."
  (funcall table nil nil 'jane-aide-fetched-p))

(require 'eieio)
(defclass jane-aide--completions ()
  ((text :initarg :text
         :type string)
   (start :initarg :start
          :type marker)
   (end :initarg :end
        :type marker)
   (table :initarg :table
          :type function))
  "A request for completions for some region.

Containing the text of the region, the start and end of the
region, and a completion table which wraps the actual AIDE
completion request.")

(defvar-local jane-aide--last-requested nil
  "The last `jane-aide--completions'.

This represents a request for completions which has been sent to
AIDE, but may not yet actually have completions.")

(defvar-local jane-aide--last-fetched nil
  "The last actually-fetched `jane-aide--completions'.

This is set in `jane-aide-completion-at-point' from
`jane-aide--last-requested' if the request has completed, and
actually has completions from AIDE.

We keep this around, separate from `jane-aide--last-requested',
so that idle completion requests won't wipe out the old
completions when the user types a character then immediately
deletes it.")

(defun jane-aide--point-in-region (req)
  "Check whether point is in the region of `jane-aide--completions' REQ.

Also checks that the region of REQ still covers exactly one or
more line(s) in the buffer; if it doesn't, it's probably not
actually the same region and we shouldn't use REQ."
  (when req
    (with-slots
     (start end)
     req
     (and
      ;; point is still in the region
      (<= start (point) end)
      ;; start is still at the start of a line; this is a cheap-to-check approximation
      ;; for "the buffer contents before start haven't changed" which is what we
      ;; really should be checking.
      (or (null (char-before start))
          (= ?\n (char-before start)))
      ;; end is at the end of a line
      (or (null (char-after end))
          (= ?\n (char-after end)))))))

(defun jane-aide--completions-request ()
  "Initiate a request for AIDE completions.

This returns a `jane-aide--completions'."
  (let (;; region start is a marker so that inserting text before the completion region
        ;; doesn't invalidate it.
        (start (copy-marker (pos-bol)))
        ;; region end is a marker so that as we insert text into the region, the region end
        ;; continues to be the end of the line.
        (end (copy-marker (pos-eol) t)))
    (jane-aide--completions
     :text (buffer-substring-no-properties start end)
     :start start :end end
     :table (jane-aide--completion-table-make))))

(declare-function company-explicit-action-p "company" ())
(defun jane-aide--completions-get ()
  "Return the `jane-aide--completions' value to use, given point and buffer.

This does most of the work for getting completions: it returns
previously-requested completions if they're still valid, or
fetches new completions if the previous ones aren't valid."
  ;; Move jane-aide--last-requested to jane-aide--last-fetched if it successfully
  ;; fetched completions.
  (when jane-aide--last-requested
    (with-slots (table) jane-aide--last-requested
                (when (jane-aide--completion-table-fetched-p table)
                  (setq jane-aide--last-fetched jane-aide--last-requested)
                  (setq jane-aide--last-requested nil))))
  (cond
   ;; Prefer to return `jane-aide--last-fetched' rather than fetch new completions, since
   ;; repeated calls to a `completion-at-point-function' should not return new completions
   ;; if the input hasn't changed.
   ((and
     (jane-aide--point-in-region jane-aide--last-fetched)
     ;; buffer text still matches some completions from this table
     ;; CR-someday sbaugh: "matching" should be based on the virtual text of the
     ;; completions, not prefix-matching like `try-completion'.
     (with-slots (start table) jane-aide--last-fetched
                 (try-completion (buffer-substring-no-properties start (point)) table)))
    jane-aide--last-fetched)
   ;; Also prefer to use `jane-aide--last-requested' if the region text hasn't changed since
   ;; we sent the request, rather than send a new request.
   ((and
     (jane-aide--point-in-region jane-aide--last-requested)
     ;; text in region is compatible with what we sent
     (with-slots (text start) jane-aide--last-requested
                 (string-prefix-p (buffer-substring-no-properties start (point)) text)))
    jane-aide--last-requested)
   ;; We might be in an idle completion; to avoid excessive AIDE load, we'll only fetch if
   ;; the user has explicitly set `jane-aide-fetch-idly'.
   ((or non-essential
        throw-on-input
        ;; CR-soon sbaugh: There are cases where company won't set non-essential even
        ;; though there was no explicit completion action; for example, when company
        ;; is updating already displayed completions in response to the user typing.
        ;; We should try to fix this.  In the meantime, we're just using a company API
        ;; to decide whether the completion was explicit or not.
        (and (bound-and-true-p company-backend) (not (company-explicit-action-p))))
    (when jane-aide-fetch-idly
      (setq-local jane-aide--last-requested (jane-aide--completions-request))))
   ;; This is an explicit request for completions.  Fetch them!
   (t
    ;; We don't need to cancel the previous `jane-aide--last-requested' because it's
    ;; automatically cancelled by creating a new request.
    (setq-local jane-aide--last-requested (jane-aide--completions-request)))))

;; CR-soon sbaugh for sbaugh: :exit-function sees the string after it's already been
;; stripped of text properties, which is where we store the completion id, so this
;; actually is a no-op.  Might be able to add a new API which works better, I have a
;; thread on this upstream.
(defun jane-aide--exit-function (string _status)
  "Log that completion STRING was chosen."
  (jane-aide--log-completion-accepted (jane-aide--client-get) string))

(defun jane-aide--annotation-function (elem &optional orig)
  "Add an <AI> tag to the end of ELEM, if it's an AI completion.

ORIG should be the original annotation function, and is called
only if ELEM is not an AI completion."
  (if (and (stringp elem) (get-text-property 0 'jane-aide-completion elem))
      " <AI>"
    (when orig
      (funcall orig elem))))

(defun jane-aide-completion-at-point ()
  "Return AIDE completions, if any, and the region they affect.

This returns a list containing the start of the completion
region, the end of the completion region, and a completion table.
This is suitable for adding to `completion-at-point-functions'."
  (unless (minibufferp)
    (when-let ((completions (jane-aide--completions-get)))
      (with-slots
       (start end table) completions
       ;; Turn the `jane-aide--completions' into the format that `completion-at-point-functions' wants.
       (list start end table
             :exit-function #'jane-aide--exit-function
             :annotation-function #'jane-aide--annotation-function
             ;; company should complete no matter how much text is before point
             :company-prefix-length t)))))

(defun jane-aide-request-ai-completion ()
  "Fetch new AIDE completions for the text at point.

This command invalidates any previously fetched completions, so
it will always return new completions."
  (interactive)
  (let ((jane-aide--last-fetched nil)
        (completion-at-point-functions '(jane-aide-completion-at-point))
        (completion-auto-help 'always))
    (call-interactively #'completion-at-point)))


(defun jane-aide-enable-idle-fetching ()
  "Enable fetching AIDE completions in idle completion."
  (interactive)
  (setq-default jane-aide-fetch-idly t))

(defun jane-aide-disable-idle-fetching ()
  "Disable fetching AIDE completions in idle completion."
  (interactive)
  (setq-default jane-aide-fetch-idly nil))


;;;; Single-symbol completion
;; See `jane-aide-add-ai-symbols-to-capf'.
(defun jane-aide--extract-symbols (aide-completions orig-start end-fn)
  "Extract symbols from AIDE-COMPLETIONS's completions.

We request completions from AIDE-COMPLETIONS that match the
current buffer up to ORIG-START, then for each of those
completions we return a substring starting at ORIG-START and
ending at whatever END-FN returns.

If `completion-at-point' later inserts one of these substrings at
ORIG-START, the AI completion which that substring was pulled
from will still be valid afterwards.

END-FN is called with no arguments, and with point at ORIG-START
in a temporary buffer.  It should return the end of the current
symbol, in a language-dependent way."
  (with-slots
   ((aide-start start) table)
   aide-completions
   (when-let* (;; If the AIDE completions haven't actually arrived yet, we just return nil;
               ;; we don't want to slow down regular completion.
               ((jane-aide--completion-table-fetched-p table))
               ;; The offset of ORIG-START within the AIDE-COMPLETIONS region.
               (offset (when (<= aide-start orig-start) (- orig-start aide-start)))
               ;; Grab AI completions which match the completion region up to ORIG-START;
               ;; not up to (point) because orig-capf might support non-prefix completion.
               (ai-strings (all-completions (buffer-substring aide-start orig-start) table)))
     (let (ret)
       ;; For each AI completion, insert it into a temp buffer and extract part of it.
       (with-temp-buffer
         (dolist (string ai-strings)
           (insert string)
           (goto-char (1+ offset))
           (push
            (buffer-substring
             (point)
             (or (ignore-error scan-error (funcall end-fn))
                 (line-end-position)))
            ret)
           (erase-buffer)))
       ret))))

(defcustom jane-aide-sort-merged-completions 'aide-first
  "How to sort merged traditional and AIDE symbol completions.

If the symbol `aide-first', completions are sorted so that AIDE
completions appear first.
If the symbol `aide-last', completions are sorted so that AIDE
completions appear last.
If nil, the list is not sorted."
  :type '(choice (const :tag "AIDE completions first" aide-first)
                 (const :tag "AIDE completions last" aide-last)
                 (const :tag "Don't sort" nil))
  :group 'jane-completion)

(defun jane-aide--display-sort-function (completions &optional orig)
  "Sort AIDE completions to the front of COMPLETIONS, after running ORIG if any."
  (let ((sorted
         ;; Run the original sorting.
         (if orig (funcall orig completions)
           ;; If there wasn't a display-sort-function originally, we sort alphabetically.
           ;; CR-soon sbaugh for sbaugh: In Emacs 29 we should be checking completions-sort.
           (sort completions #'string-lessp))))
    (if jane-aide-sort-merged-completions
        (sort
         sorted
         (cond
          ((eq jane-aide-sort-merged-completions 'aide-first)
           (lambda (a b)
             (and (get-text-property 0 'jane-aide-completion a)
                  (not (get-text-property 0 'jane-aide-completion b)))))
          ((eq jane-aide-sort-merged-completions 'aide-last)
           (lambda (a b)
             (and (not (get-text-property 0 'jane-aide-completion b))
                  (get-text-property 0 'jane-aide-completion a))))
          (t (error "Unknown value %s" jane-aide-sort-merged-completions))))
      sorted)))

;; CR-soon azeng for sbaugh: Report this bug in `completion-table-merge' upstream.
(defun jane-aide--merge-try-completion (string &rest trys)
  "Merge `try-completion's TRYS according to STRING.

TRYS should be a list of values which can be returned from
`try-completion'.

This function is mostly stolen from `completion-table-merge', but
that has a bug."
  (if (member string trys)
      string
    ;; Key difference from `completion-table-merge': we don't pass pred here, because here
    ;; it's being run on strings instead of completion table elements (such as symbols).
    ;; This is just a bug in completion-table-merge.
    (try-completion string
                    (mapcar (lambda (value)
                              (if (eq value t) string value))
                            (delq nil trys)))))

(defun jane-aide--merged-symbol-completion-table (aide-completions
                                                  orig-capf end-fn
                                                  string pred action)
  "A completion table combining AIDE-COMPLETIONS and ORIG-CAPF.

STRING, PRED, and ACTION are the programmed completion table
arguments; see Info node `(elisp)Programmed Completion'.

We grab the AI completions from AIDE-COMPLETIONS with END-FN and
`jane-aide--extract-symbols', change the metadata of ORIG-CAPF's
table to prioritize those AI completions, and then do the
completion action specified by STRING, PRED, and ACTION on both
the AI completions and the traditional completions, merging the
result.

If AIDE-COMPLETIONS is empty, this will be a no-op, since we'll
extract no symbols from it."
  (let ((orig-start (nth 0 orig-capf))
        (orig-table (nth 2 orig-capf)))
    (cond
     ((eq action 'metadata)
      ;; Modify the metadata of ORIG-CAPF, replacing annotation-function and
      ;; display-sort-function with versions which prioritize AI completions.
      (let ((md (cdr (completion-metadata string orig-table pred))))
        (cl-flet ((replacement-for
                   (symbol func)
                   ;; If there's no original, put in the named function instead of a
                   ;; lambda, for better debuggability.
                   (cons symbol (if-let ((orig (alist-get symbol md)))
                                    (lambda (x) (funcall func x orig))
                                  func))))
          (append
           (list
            'metadata
            ;; CR-soon sbaugh for sbaugh: We should check for :annotation-function and
            ;; :display-sort-function in the plist in ORIG-CAPF as well; otherwise this
            ;; table-level metadata will override those.
            (replacement-for 'annotation-function #'jane-aide--annotation-function)
            (replacement-for 'display-sort-function #'jane-aide--display-sort-function))
           md))))
     ((member action '(t lambda nil))
      ;; These are actual completion actions, so we need to extract the AI completions.
      (let ((aide-symbol-table (jane-aide--extract-symbols aide-completions orig-start end-fn)))
        ;; Now to run the completion actions against both sources and merge the results.
        ;; Note that we never pass PRED for the AI completions; PRED can only be validly
        ;; used with ORIG-CAPF's table.  Since we omit the predicate, we'll just always
        ;; include all AI completions - seems fine.
        (cond
         ((eq action 't)
          (append (all-completions string aide-symbol-table)
                  (all-completions string orig-table pred)))
         ((eq action 'lambda)
          (or (test-completion string aide-symbol-table)
              (test-completion string orig-table pred)))
         ((eq action 'nil)
          (jane-aide--merge-try-completion
           string
           (try-completion string aide-symbol-table)
           (try-completion string orig-table pred))))))
     ;; Forward any other actions to the original table.
     (t (if (functionp orig-table)
            (funcall orig-table string pred action)
          (complete-with-action action orig-table string pred))))))

(defun jane-aide-add-ai-symbols-to-capf (orig-capf end-fn)
  "Extract symbols from AI completions, and add them to ORIG-CAPF if any.

If `jane-aide-completion-at-point' is present in
`completion-at-point-functions', then as the cursor moves and
text gets inserted, `completion-at-point' will switch between the
AIDE completions and the single-symbol traditional
completions (e.g. merlin and LSP).  This may be annoying, because
the traditional completions will prevent doing completion with
the AI completion that the user might actually want.

The solution is straightforward: We include AI completions with
the traditional completions.  When doing traditional completion
at some position, we extract expressions from the AI completions
which also make sense to insert at that position, and combine
them with the traditional completions.

Specifically, in this function, given the return value ORIG-CAPF
from a traditional completion function, we replace its completion
table with one which will also include AI completions.

END-FN is passed down to `jane-aide--extract-symbols'.  It should
return the end of the symbol at point."
  (when orig-capf
    (if-let* ((aide-completions
               ;; Since this request happens even if the user doesn't intend to get AI
               ;; completions, it's basically idle completion, so we bind non-essential to
               ;; avoid fetching unless jane-aide-fetch-idly is non-nil.
               (let ((non-essential t)) (jane-aide--completions-get))))
        ;; Replace the third element of ORIG-CAPF: the completion table.
        (append
         (list
          (nth 0 orig-capf)
          (nth 1 orig-capf)
          ;; A new completion table which combines both orig-table and aide-table.
          (apply-partially #'jane-aide--merged-symbol-completion-table
                           aide-completions orig-capf end-fn))
         (nthcdr 3 orig-capf))
      orig-capf)))

;;;; Configuration
;; Enable the AIDE capfs globally.
(add-hook 'completion-at-point-functions #'jane-aide-completion-at-point 50)
;; Elisp single-symbol configuration.
(defun jane-aide--elisp-completion-at-point-end-fn ()
  "Return the end of the current sexp, or nil."
  (ignore-error scan-error
    (save-excursion (forward-sexp) (point))))

(define-advice elisp-completion-at-point (:filter-return (orig-capf) with-aide-symbols)
  (jane-aide-add-ai-symbols-to-capf orig-capf #'jane-aide--elisp-completion-at-point-end-fn))

;; Merlin single-symbol configuration.
(declare-function merlin-bounds-of-ocaml-atom-at-point "merlin" ())
(defun jane-aide--ocaml-completion-at-point-end-fn ()
  "Return the end of the current OCaml atom, or nil."
  ;; CR-someday sbaugh: Should use tree-sitter here.
  (let ((bounds (merlin-bounds-of-ocaml-atom-at-point)))
    (when bounds
      (cdr bounds))))

(define-advice merlin-completion-at-point (:filter-return (orig-capf) with-aide-symbols)
  (jane-aide-add-ai-symbols-to-capf orig-capf #'jane-aide--ocaml-completion-at-point-end-fn))

;; CR-soon sbaugh: We'd like to use `jane-aide-add-ai-symbols-to-capf' on
;; `eglot-completion-at-point' too, but Eglot has an exit-function which expects to be
;; called on one of the completions returned by Eglot.  We could wrap the exit-function,
;; but exit-function doesn't get the text properties on the completion so it's hard to
;; tell if a given completion is from AIDE or not.  So for now it's Merlin-only.

(provide 'jane-aide)
;;; jane-aide.el ends here
