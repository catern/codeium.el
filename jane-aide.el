;;; jane-aide.el --- AIDE support                    -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Jane Street Capital

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
(declare-function jane-aide--start "ecaml_plugin" (version logdir mode prog) t)
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

Must be either `prod', `dev' or an absolute path to an AIDE exe.
Changing this will not take effect without restarting the AIDE
client.  You can restart the AIDE client with
\\[jane-aide-client-restart]."
  :type '(choice (const :tag "Prod" prod)
                 (const :tag "Dev" dev)
                 (file :tag "Custom exe"))
  :group 'jane-aide)

(defun jane-aide--client-get ()
  "Get `jane-aide--client', creating it if necessary."
  (unless jane-aide--client
    (let ((log-dir (expand-file-name "aide" user-emacs-directory))
          (devprod (if (stringp jane-aide-mode) 'dev jane-aide-mode))
          (prog (if (stringp jane-aide-mode) jane-aide-mode nil)))
      (make-directory log-dir t)
      (setq jane-aide--client
            (jane-aide--start
             (format "%s %s" (emacs-version) (jane-ecaml-version)) log-dir devprod prog))))
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

(defun jane-aide-client-restart-feature-local (project)
  "Set `jane-aide-mode' to the AIDE binary from PROJECT then restart AIDE.

Interactively, PROJECT is the current project."
  (interactive (list (project-current t)))
  (let ((aide-path (file-name-concat (project-root project) "app/aide/bin/main.exe")))
    (unless (file-exists-p aide-path)
      (error "AIDE binary not found in %s. Maybe you need to build it? (Tried %s)" project aide-path))
    (setopt jane-aide-mode aide-path)
    (jane-aide-client-restart)
    (message "AIDE binary set to %s. Restarted AIDE client." aide-path)))

(defconst jane-aide--bos-whitespace-regex (rx bos (+ (any " \t")))
  "A regex matching non-newline whitespace at the beginning of a string.")

(defun jane-aide--get-text-parts (orig-text completions)
  "Split ORIG-TEXT on the virtual text offsets from all COMPLETIONS.

Returns a list of non-empty strings which, if concatenated,
produce ORIG-TEXT.  Except that any whitespace will be stripped
from the beginning of it.

The buffer text should contain all of those strings, in order, to
be compatible with any completion from COMPLETIONS.

Put another way: each completion \"wants\" all of these strings
to appear in the buffer text, for that completion to be valid."
  (let ((last-offset 0)
        (all-offsets
         (mapcan
          (lambda (comp) (mapcar #'cdr (get-text-property 0 'jane-aide-completion-virtual-text comp)))
          completions))
        parts)
    ;; We iterate through the virtual text offsets of all the completions, splitting at *all* of them
    (dolist (offset (sort all-offsets #'<))
      (cl-assert (<= offset (length orig-text)) t "Virtual text longer than original text: %s" orig-text)
      ;; Don't "split" twice at the same offset, that will just give us an empty string.
      (unless (= last-offset offset)
        (push (substring orig-text last-offset offset) parts))
      (setq last-offset offset))
    (unless (= last-offset (length orig-text))
      (push (substring orig-text last-offset) parts))
    (setq parts (nreverse parts))
    ;; If a completion only *inserts* text, the completion is valid only if the buffer
    ;; contains every substring in PARTS.  It's also possible for a completion to *delete*
    ;; text.  Such completions should be valid when the user has deleted the same text,
    ;; even though that means the buffer no longer contains every substring in PARTS.
    ;;
    ;; This is hard to handle in full generality, but AIDE gives us a guarantee that the
    ;; only text that will ever be deleted by completions is whitespace at the start of
    ;; the completion region.  If we delete that whitespace from PARTS now, then we won't
    ;; require it to appear in the buffer, and such unindenting completions will be
    ;; considered valid even if the user has deleted that whitespace.
    (while (and parts (string-match jane-aide--bos-whitespace-regex (car parts)))
      (setf (car parts) (substring (car parts) (match-end 0)))
      ;; If we made a string empty, pop it off.
      (when (string-empty-p (car parts))
        (pop parts)))
    parts))

(cl-defstruct jane-aide-document
  "Structure representing a document for AIDE.

FILE-PATH: The full path of the buffer file.
CONTENTS: The buffer content as a string.
LANGUAGE: The name of the detected programming language.
EOL-TYPE: The end-of-line type (0 for LF, 1 for CRLF).
MODIFIED: Non-nil if the buffer has been modified."
  (file-path nil :type string)
  (contents nil :type string)
  (language nil :type string)
  (eol-type nil :type integer)
  (modified nil :type boolean))

(defun jane-aide--make-document (buffer)
  "Create a value of type `jane-aide-document' from BUFFER."
  (with-current-buffer buffer
    (make-jane-aide-document
     :file-path buffer-file-name
     :contents (buffer-substring-no-properties (point-min) (point-max))
     :language (cond
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
     :eol-type (let ((eol-type (coding-system-eol-type buffer-file-coding-system)))
                 (if (vectorp eol-type) 0 eol-type))
     :modified (buffer-modified-p))))

(defun jane-aide--offset-of-pos (position)
  "Return the byte offset in the current buffer corresponding to POSITION."
  (- (position-bytes position) (position-bytes (point-min))))

(defun jane-aide--completion-table-make (orig-text)
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

ORIG-TEXT should be the original text that this request was made for.

This function is the only thing that would need to be replaced to
switch to a different backend."
  (let* ((document (jane-aide--make-document (current-buffer)))
         (request
          ;; CR-someday nfarlow: Consider changing `jane-aide--request-start' to accept a
          ;; document struct instead of individual arguments
          (jane-aide--request-start
           (jane-aide--client-get)
           (when-let ((proj (project-current)))
             (project-root proj))
           (jane-aide-document-file-path document)
           (jane-aide-document-language document)
           (jane-aide-document-eol-type document)
           (jane-aide-document-contents document)
           ;; Pass the byte offset of (point) in the buffer text.
           (jane-aide--offset-of-pos (point))))
         waited-for-completions completions parts)
    (lambda (string pred action)
      (cl-case action
        ;; CR-someday sbaugh: In Emacs 30 the category can be set by
        ;; `jane-aide-completion-at-point'; we should move it there, which will make it
        ;; easier to replace `jane-aide--completion-table-make'.
        (metadata
         '(metadata
           (category . aide)
           ;; AIDE returns completions in order from best to worst, so don't sort them.
           (display-sort-function . identity)))
        (jane-aide-fetched-p (jane-aide--request-finished-p request))
        (t
         (unless waited-for-completions
           ;; CR-someday sbaugh: The request should raise an error if it was cancelled; we should
           ;; detect cancellation and turn it into a nil instead.  Or maybe re-request?  Either
           ;; way, hard to test since we can't actually get a cancellation right now.

           ;; CR-someday nfarlow: Once ECaml's block_on_async will allow timers to run, we
           ;; can just use 'jane-aide--request-wait' here.  See https://jira/browse/TANDC-3345
           (setq completions (progn (while (not (jane-aide--request-finished-p request))
                                      (accept-process-output nil 0.001))
                                    (jane-aide--request-wait request)))
           (setq parts (jane-aide--get-text-parts orig-text completions))
           (dolist (comp completions)
             ;; Remove the virtual text property since we don't need it anymore and it's noisy.
             (remove-text-properties 0 (length comp) '(jane-aide-completion-virtual-text) comp))
           (setq waited-for-completions t))
         (if (eq action 'jane-aide-parts)
             parts
           (complete-with-action action completions string pred)))))))

(defun jane-aide--offsets-of-parts (string parts)
  "Get offsets of PARTS in STRING.

Returns the position in STRING where each substring in PARTS
starts.

If a substring doesn't appear in STRING, its entry in the list is
nil."
  (let ((last-offset 0)
        ret)
    (dolist (part parts)
      (let ((pos (string-search part string last-offset)))
        (push pos ret)
        (when pos
          (setq last-offset (+ pos (length part))))))
    (nreverse ret)))

(defun jane-aide--to-pattern-with-parts (string parts point)
  "Convert STRING into a PCM pattern with PARTS and POINT.

Returns nil if PARTS are not substrings of STRING, in order.

The resulting pattern can contain a wildcard at the start of each
string in PARTS; only positions which are after POINT actually
have a wildcard added.

A PCM pattern is a list of strings and symbols.  Each symbol
represents a wildcard which behaves the same with
`completion-all-completion', but has different behavior with
`completion-try-completion'.  See the docstring of
`completion-pcm--merge-completions' for the meaning of the
symbols."
  ;; `jane-aide--get-text-parts' deletes whitespace from the start of PARTS so that it's
  ;; not required to be in STRING.  Now delete the same whitespace from the start of
  ;; STRING before turning it into a pattern.
  (when (string-match jane-aide--bos-whitespace-regex string)
    (setf string (substring string (match-end 0)))
    ;; Adjust point for the missing whitespace.
    (setq point (max 0 (- (+ point (match-beginning 0)) (match-end 0)))))
  (let ((part-offsets (jane-aide--offsets-of-parts string parts)))
    ;; Check that each part appeared in STRING; if one is missing, just return nil.
    (when (cl-notany #'null part-offsets)
      (let ((last-offset 0)
            pattern)
        ;; The pattern starts with the symbol `prefix' to support substring-like behavior.
        (push 'prefix pattern)
        (dolist (offset
                 ;; `seq-uniq' dedups the offsets, so we don't insert multiple wildcards at one place.
                 (seq-uniq (sort (cons point part-offsets) #'<)))
          ;; Add the string before this offset
          (push (substring string last-offset offset) pattern)
          (setq last-offset offset)
          ;; Add some kind of wildcard
          (cond
           ((= offset point)
            ;; Hack: Ignore whitespace right before point and after a newline, which we assume was inserted by
            ;; auto-indentation.  Inserting text in the buffer which isn't present in a completion
            ;; *usually* indicates that the user disagrees with that completion, so therefore that
            ;; completion shouldn't be suggested.  However, auto-indentation happens
            ;; automatically; the user may not actually *want* to indent.  So, we still want to
            ;; suggest completions that don't include that indentation.  Note: This is completely
            ;; different from `jane-aide--get-text-parts' deleting whitespace from the *start* of
            ;; the string.
            (when (stringp (car pattern))
              (when (string-match (rx "\n" (group (+ (any " \t"))) eos) (car pattern))
                (setf (car pattern) (substring (car pattern) 0 (match-beginning 1)))))
            (push 'point pattern))
           ((< point offset)
            ;; Hack: To accomodate `company-preview-frontend' (which can't display
            ;; completions which insert text before point) only include wildcards that are
            ;; after point, meaning we won't match completions which want to insert text
            ;; before point.
            (push 'any pattern))))
        (push (substring string last-offset) pattern)
        ;; We built the pattern reversed through `push'; reverse it so it's in the correct order.
        (completion-pcm--optimize-pattern (nreverse pattern))))))

(defun jane-aide--to-pcm-pattern (string table point)
  "Convert STRING and POINT into a PCM pattern to run against TABLE.

Returns nil if STRING is not compatible with TABLE.

Doesn't check `completion-boundaries' since AIDE doesn't use it."
  ;; CR-someday sbaugh: Probably the completion table action should be "given a string, return a
  ;; list of offsets where fuzzy completion should happen in this string".  That's a more generic
  ;; capability. That's harder to iterate on though, so will do that once this is more settled.
  (let ((parts (funcall table nil nil 'jane-aide-parts)))
    (jane-aide--to-pattern-with-parts string parts point)))

(defun jane-aide--get-accepted-offset (completion)
  "Get the offset in COMPLETION to move point to after accepting it."
  (let* ((byte-offset-from-start (get-text-property 0 'jane-aide-completion-cursor-offset completion))
         (bytes-after-offset (substring (string-to-unibyte completion) byte-offset-from-start))
         (length-after-offset (length (decode-coding-string bytes-after-offset 'utf-8-unix))))
    (- (length completion) length-after-offset)))

(defun jane-aide-try-completion (string table pred point)
  "`completion-try-completion' on STRING, TABLE, PRED, POINT for the AIDE style."
  (when-let ((pattern (jane-aide--to-pcm-pattern string table point)))
    (let ((all (completion-pcm--all-completions "" pattern table pred)))
      (if-let ((sole (and (length= all 1) (car all))))
          ;; We're compatible with exactly one completion, so don't bother calling
          ;; `completion-pcm--merge-try'; we're returning that completion or `t'.
          (let ((sole-offset (jane-aide--get-accepted-offset sole)))
            (if (and
                 ;; Our pattern ignores some whitespace, so STRING might not be exactly the same as
                 ;; the one completion.  Only return t if it is.  (`completion-pcm--merge-try'
                 ;; doesn't do this check, so it can erroneously return t for our patterns)
                 (string-equal string sole)
                 ;; Also, only return t if we don't want to move point.
                 (= (length sole) sole-offset))
                t
              (cons sole sole-offset)))
        ;; CR-soon sbaugh for sbaugh: This returns an updated value of point based on the
        ;; pattern we provide, which might have whitespace stripped from the start, so it
        ;; may move point incorrectly.  (We should probably have tests which just call
        ;; `jane-aide-try-completion' directly)
        (completion-pcm--merge-try pattern all "" "")))))

(defun jane-aide-all-completions (string table pred point)
  "`completion-all-completions' on STRING, TABLE, PRED, POINT for the AIDE style."
  (when-let ((pattern (jane-aide--to-pcm-pattern string table point)))
    (let ((all (completion-pcm--all-completions "" pattern table pred)))
      (completion-pcm--hilit-commonality pattern all))))

(setf (alist-get 'aide completion-category-defaults) '((styles aide)))
(setf (alist-get 'aide completion-styles-alist) '(jane-aide-try-completion jane-aide-all-completions))

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

(cl-defstruct jane-aide--completions
  "A request for completions for some region.

Containing the text of the region, the start and end of the
region, and a completion table which wraps the actual AIDE
completion request."
  (text nil :type string)
  (start nil :type marker)
  (end nil :type marker)
  (table nil :type function))

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
    (let ((start (jane-aide--completions-start req))
          (end (jane-aide--completions-end req)))
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
  (let* (;; region start is a marker so that inserting text before the completion region
         ;; doesn't invalidate it.
         (start (copy-marker (pos-bol)))
         ;; region end is a marker so that as we insert text into the region, the region end
         ;; continues to be the end of the line.
         (end (copy-marker (pos-eol) t))
         (text (buffer-substring-no-properties start end)))
    ;; CR-soon sbaugh: Stick all the local variables of the completion table into this
    ;; structure, and pass it in to the completion table.  That makes the completion table
    ;; more debuggable.  (And add a table action to grab this structure, perhaps)
    (make-jane-aide--completions
     :text text
     :start start :end end
     :table (jane-aide--completion-table-make text))))

(declare-function company-explicit-action-p "company" ())
(defun jane-aide--completions-get ()
  "Return the `jane-aide--completions' value to use, given point and buffer.

This does most of the work for getting completions: it returns
previously-requested completions if they're still valid, or
fetches new completions if the previous ones aren't valid."
  ;; Move jane-aide--last-requested to jane-aide--last-fetched if it successfully
  ;; fetched completions.
  (when jane-aide--last-requested
    (let ((table (jane-aide--completions-table jane-aide--last-requested)))
      (when (jane-aide--completion-table-fetched-p table)
        (setq jane-aide--last-fetched jane-aide--last-requested)
        (setq jane-aide--last-requested nil))))
  (cond
   ;; Prefer to return `jane-aide--last-fetched' rather than fetch new completions, since
   ;; repeated calls to a `completion-at-point-function' should not return new completions
   ;; if the input hasn't changed.
   ((and
     (jane-aide--point-in-region jane-aide--last-fetched)
     ;; Buffer text still matches some completions from this table, according to our completion style.
     (let ((start (jane-aide--completions-start jane-aide--last-fetched))
           (end (jane-aide--completions-end jane-aide--last-fetched))
           (table (jane-aide--completions-table jane-aide--last-fetched)))
       (jane-aide-all-completions (buffer-substring-no-properties start end) table nil
                                  (- (point) start))))
    jane-aide--last-fetched)
   ;; Also prefer to use `jane-aide--last-requested' if the region text hasn't changed since
   ;; we sent the request, rather than send a new request.
   ((and
     (jane-aide--point-in-region jane-aide--last-requested)
     ;; text in region is compatible with what we sent
     (let ((start (jane-aide--completions-start jane-aide--last-requested))
           (text (jane-aide--completions-text jane-aide--last-requested)))
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

(defun jane-aide--exit-function (string status)
  "Log that completion STRING was chosen.

Also, if STATUS is the symbol `finished', move point to the
offset in the completion requested by AIDE."
  ;; CR-someday nfarlow: Company doesn't call `completion-try-completion'. For now, we'll
  ;; set the new cursor position in this exit function. This is a bit sad since you can
  ;; sometimes see the cursor jumping back from the end of the line.

  ;; Only move point if completion is completely done; see (info "(elisp) Completion Variables")
  (when (eq status 'finished)
    ;; Assume the cursor is at the end of string. Move it back to the correct offset, with
    ;; the complication that AIDE provides a byte offset not a character offset.
    (let* ((byte-offset-from-start (get-text-property 0 'jane-aide-completion-cursor-offset string))
           (bytes-after-offset (substring (string-to-unibyte string) byte-offset-from-start))
           (chars-backward (length (decode-coding-string bytes-after-offset 'utf-8-unix))))
      (backward-char chars-backward)))
  (jane-aide--log-completion-accepted (jane-aide--client-get) string))

(defun jane-aide--annotation-function (_candidate)
  "Add an <AI> tag to the end of every completion."
  " <AI>")

(defun jane-aide-completion-at-point ()
  "Return AIDE completions, if any, and the region they affect.

This returns a list containing the start of the completion
region, the end of the completion region, and a completion table.
This is suitable for adding to `completion-at-point-functions'."
  (unless (minibufferp)
    (when-let ((completions (jane-aide--completions-get)))
      ;; Turn the `jane-aide--completions' into the format that `completion-at-point-functions' wants.
      (list (jane-aide--completions-start completions)
            (jane-aide--completions-end completions)
            (jane-aide--completions-table completions)
            :exit-function #'jane-aide--exit-function
            :annotation-function #'jane-aide--annotation-function
            :company-use-while-no-input t
            ;; company should complete no matter how much text is before point
            :company-prefix-length t))))

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
  (add-hook 'completion-at-point-functions #'jane-aide-completion-at-point 50)
  (setq-default jane-aide-fetch-idly t))

(defun jane-aide-disable-idle-fetching ()
  "Disable fetching AIDE completions in idle completion."
  (interactive)
  ;; When a user is switching idle fetching of completions on and off, they may find it
  ;; confusing that the cached completions are still visible after turning idle fetching
  ;; off.  So clear them in the current buffer, to try to avoid that confusion.
  (setq jane-aide--last-fetched nil)
  (setq jane-aide--last-requested nil)
  (remove-hook 'completion-at-point-functions #'jane-aide-completion-at-point)
  (setq-default jane-aide-fetch-idly nil))



;;;; Single-symbol completion
;; See `jane-aide-add-ai-symbols-to-capf'.
(defun jane-aide--merged-compute-is-ai-suggestion (aide-completions position string)
  "Compute whether STRING at POSITION is suggested by AIDE-COMPLETIONS.

That is, if we insert STRING at POSITION in the current buffer,
will there still be at least one AI completion that applies?

AIDE-COMPLETIONS should be of type `jane-aide--completions'."
  (let ((aide-start (jane-aide--completions-start aide-completions))
        (aide-table (jane-aide--completions-table aide-completions)))
    (when (<= aide-start position)
      (let (;; This is what will be in the AIDE region if we insert STRING
            (aide-region-text (concat (buffer-substring aide-start position) string)))
        ;; If any AIDE completions will still apply after we insert STRING, return t.
        ;; (We use `all-completions' rather than `jane-aide-all-completions' for speed.)
        (not (null (all-completions aide-region-text aide-table)))))))

(defun jane-aide--merged-completions-get-idly ()
  "Call `jane-aide--completions-get' as if we're doing idle completion.

Additionally returns nil if the completions haven't been fetched yet."
  (let ((non-essential t)) (jane-aide--completions-get)))

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

(defun jane-aide--merged-is-ai-suggestion (candidate)
  "Return non-nil if CANDIDATE was suggested by AIDE."
  ;; Look at the end of the string for the text property to be correct with
  ;; partial-completion. (see comment in `merlin-cap--annotate')
  (get-text-property (1- (length candidate)) 'jane-aide-merged-completion candidate))

(defun jane-aide--merged-append-ai-tag (_completion annotation)
  "Append \"<AI>\" to ANNOTATION.

COMPLETION is unused.  This function exists for testing, to
easily see which completions are getting tagged."
  (concat annotation " <AI>"))

(defun jane-aide--merged-annotation-function (elem &optional orig)
  "Append \"<AI>\" to ELEM's annotation if AIDE suggested it.

ORIG should be the original annotation function, if any."
  (let ((orig-annotation (when orig (funcall orig elem))))
    (if (jane-aide--merged-is-ai-suggestion elem)
        (jane-aide--merged-append-ai-tag elem orig-annotation)
      orig-annotation)))

(defun jane-aide--merged-display-sort-function (completions &optional orig)
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
          ;; CR-someday sbaugh: The AI completions are sorted by quality; we should sort an
          ;; orig-completion which matches a higher-quality AI completion earlier.
          ((eq jane-aide-sort-merged-completions 'aide-first)
           (lambda (a b)
             (and (jane-aide--merged-is-ai-suggestion a)
                  (not (jane-aide--merged-is-ai-suggestion b)))))
          ((eq jane-aide-sort-merged-completions 'aide-last)
           (lambda (a b)
             (and (not (jane-aide--merged-is-ai-suggestion b))
                  (jane-aide--merged-is-ai-suggestion a))))
          (t (error "Unknown value %s" jane-aide-sort-merged-completions))))
      sorted)))

(defun jane-aide--merged-symbol-completion-table (orig-capf
                                                  string pred action)
  "A completion table adding AI annotations to ORIG-CAPF.

STRING, PRED, and ACTION are the programmed completion table
arguments; see Info node `(elisp)Programmed Completion'.

We change the metadata of ORIG-CAPF's table to annotate
completions which are suggested by AIDE, and sort them first."
  (let ((orig-table (nth 2 orig-capf)))
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
            (replacement-for 'annotation-function #'jane-aide--merged-annotation-function)
            (replacement-for 'display-sort-function #'jane-aide--merged-display-sort-function))
           md))))
     ((eq action 't)                    ; all-completions
      ;; Get the list of all completions from ORIG-TABLE
      (let ((orig-completions (all-completions string orig-table pred)))
        (when-let ((aide-completions (jane-aide--merged-completions-get-idly))
                   ;; If the completions haven't been fetched already, don't wait for them.
                   ((jane-aide--completion-table-fetched-p (jane-aide--completions-table aide-completions)))
                   ;; Calculating completion-position and completion-base are the parts of this function
                   ;; which actually depend on ORIG-CAPF or STRING.  Other than those, we could just call
                   ;; `jane-aide--merged-compute-is-ai-suggestion' inside
                   ;; `jane-aide--merged-annotation-function' and
                   ;; `jane-aide--merged-display-sort-function', and not bother with the text property.
                   (completion-position
                    ;; The position where these completions will be inserted if one of them is chosen.
                    (nth 0 orig-capf))
                   (completion-base
                    ;; `all-completions' returns strings which replace the part of STRING inside
                    ;; `completion-boundaries'; completion-base is the part before that, so the full
                    ;; completion is (concat completion-base comp)
                    (let ((boundaries (completion-boundaries string orig-table pred "")))
                      (substring string 0 (car boundaries)))))
          (dolist (comp orig-completions)
            ;; If AI completion likes COMP...
            (when (jane-aide--merged-compute-is-ai-suggestion
                   aide-completions completion-position (concat completion-base comp))
              ;; ...then mark COMP with a text property so we annotate it with <AI> and sort it first.
              (put-text-property 0 (length comp) 'jane-aide-merged-completion t comp))))
        orig-completions))
     ;; Forward any other actions to the original table.
     (t (if (functionp orig-table)
            (funcall orig-table string pred action)
          (complete-with-action action orig-table string pred))))))

(defun jane-aide-add-ai-symbols-to-capf (orig-capf)
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
table with one which will also include AI completions."
  (when orig-capf
    ;; Only wrap ORIG-CAPF if we actually have some AI completions to use.
    (if (jane-aide--merged-completions-get-idly)
        ;; Replace the third element of ORIG-CAPF: the completion table.
        (append
         (list
          (nth 0 orig-capf)
          (nth 1 orig-capf)
          ;; A new completion table which wraps the original one.
          (apply-partially #'jane-aide--merged-symbol-completion-table orig-capf))
         (nthcdr 3 orig-capf))
      orig-capf)))

;;;; Tests
;; We use ERT to test jane-aide since it eases external collaboration and will make it
;; easier to (maybe, eventually) publish this as an alternative to codeium.el
(defmacro jane-aide--with-test-buffer (&rest body)
  "Run BODY with a temp buffer set up for mocked AIDE completion."
  `(with-temp-buffer
     (setq-local completion-at-point-functions '(jane-aide-completion-at-point))
     ;; Ensure that only the aide completion style is considered
     (setq-local completion-styles '(aide))
     (cl-letf (((symbol-function 'jane-ecaml-version) #'ignore)
               ((symbol-function 'jane-aide--start) #'ignore)
               ((symbol-function 'jane-aide--stop) #'ignore)
               ((symbol-function 'jane-aide--log-completion-accepted) #'ignore)
               ((symbol-function 'jane-aide--request-finished-p) #'always)
               ((symbol-function 'jane-aide--request-wait) #'identity))
       ,@body)))

(defun jane-aide--list-to-completion (parts)
  "Concatenate the elements of PARTS to form a jane-aide completion string.

Each element should be either:
- \"some string\" (the original text in the buffer)
- (\"some string\") (virtual text added to the buffer)
- point (where point should go after the completion; defaults to
  the end of the completion if not present."
  (let (virtual-text full-text cursor-offset)
    (let ((non-virtual-offset 0))
      (dolist (part parts)
        (cond
         ((listp part)
          (cl-assert (= (length part) 1))
          (let ((string (car part)))
            (push (cons string non-virtual-offset) virtual-text)
            (setq full-text (concat full-text string))))
         ((eq part 'point)
          (setq cursor-offset (length full-text)))
         ((stringp part)
          (setq non-virtual-offset (+ non-virtual-offset (length part)))
          (setq full-text (concat full-text part)))
         (t
          (error "Can't handle %s" part)))))
    (setq virtual-text (nreverse virtual-text))
    (unless cursor-offset
      (setq cursor-offset (length full-text)))
    (propertize full-text
                'jane-aide-completion t
                'jane-aide-completion-virtual-text virtual-text
                'jane-aide-completion-cursor-offset cursor-offset)))

(require 'ert)
(defun jane-aide--test-complete (prefix suffix expected-prefix expected-suffix completions)
  "Trigger completion with point between PREFIX and SUFFIX and compare results.

EXPECTED-PREFIX and EXPECTED-SUFFIX are what should be before and
after point after completion.

COMPLETIONS is a list of completions, each passed to
`jane-aide--list-to-completion'."
  (let ((completions (mapcar #'jane-aide--list-to-completion completions)))
    (cl-letf (((symbol-function 'jane-aide--request-start) (lambda (&rest _) completions)))
      (insert prefix)
      (save-excursion (insert suffix))
      (completion-at-point)
      (let ((actual-prefix (buffer-substring (point-min) (point)))
            (actual-suffix (buffer-substring (point) (point-max))))
        (should (equal (list actual-prefix actual-suffix)
                       (list expected-prefix expected-suffix))))
      (erase-buffer))))

;; CR-someday sbaugh: We should also have tests for merged completion.
(ert-deftest test-aide-completion-one-shot ()
  (jane-aide--with-test-buffer
   ;; Simple end of line completion
   (jane-aide--test-complete
    "let " ""
    "let x = 2" ""
    '(("let " ("x = 2"))))
   ;; Un-indent
   (jane-aide--test-complete
    "    " ""
    "end" ""
    '((("end"))))
   ;; Completion in the middle of a line
   (jane-aide--test-complete
    "let items = [" "]"
    "let items = [1; 2; 3]" ""
    '(("let items = [" ("1; 2; 3") "]")))
   ;; Completion in the middle of a line that changes suffix
   (jane-aide--test-complete
    "let foo = [" "]"
    "let foo = [1; 2; 3] in" ""
    '(("let foo = [" ("1; 2; 3") "]" (" in"))))
   ;; Completion after whitespace
   (jane-aide--test-complete
    "foo    " ""
    "foo    bar" ""
    '(("foo    " ("bar"))))
   ;; Completion on an empty line
   (jane-aide--test-complete
    "" ""
    "let () = ()" ""
    '((("let () = ()"))))
   (jane-aide--test-complete
    "type t = " ""
    "type t = {" "}"
    '(("type t = " ("{") point ("}"))))
   ))

(ert-deftest test-aide-to-pattern ()
  ;; multiple parts and various locations of point
  (should (equal (jane-aide--to-pattern-with-parts "foobar" '("oo" "a") 6)
                 '(prefix "f" "oob" "ar")))
  (should (equal (jane-aide--to-pattern-with-parts "foobar" '("oo" "a") 5)
                 '(prefix "f" "oob" "a" point "r")))
  (should (equal (jane-aide--to-pattern-with-parts "foobar" '("oo" "a") 1)
                 '(prefix "f" point "oob" any "ar")))
  (should (equal (jane-aide--to-pattern-with-parts "foobar" '("oo" "a") 0)
                 '(prefix point "f" any "oob" any "ar")))
  ;; stripping whitespace from the start; point is updated correctly
  (should (equal (jane-aide--to-pattern-with-parts "  foobar" '("oo" "a") 0)
                 '(prefix point "f" any "oob" any "ar")))
  (should (equal (jane-aide--to-pattern-with-parts "  foobar" '("oo" "a") 1)
                 '(prefix point "f" any "oob" any "ar")))
  (should (equal (jane-aide--to-pattern-with-parts "  foobar" '("oo" "a") 6)
                 '(prefix "f" "oob" point "ar")))
  (should (equal (jane-aide--to-pattern-with-parts "  foobar" '("oo" "a") 8)
                 '(prefix "f" "oob" "ar")))
  ;; Whitespace is stripped from the end after a newline and the newline is preserved.
  (should (equal (jane-aide--to-pattern-with-parts "foobar\n  " '("oo" "a") 0)
                 '(prefix point "f" any "oob" any "ar\n  ")))
  ;; parts at the start
  (should (equal (jane-aide--to-pattern-with-parts "foobar" '("foo" "a") 6)
                 '(prefix "foob" "ar")))
  ;; parts at the start, and point there too
  (should (equal (jane-aide--to-pattern-with-parts "foobar" '("foo" "a") 0)
                 '(prefix point "foob" any "ar")))
  ;; parts at the end
  (should (equal (jane-aide--to-pattern-with-parts "foobar" '("oo" "ar") 0)
                 '(prefix point "f" any "oob" any "ar")))
  ;; parts at the start and end
  (should (equal (jane-aide--to-pattern-with-parts "foobar" '("foo" "ar") 0)
                 '(prefix point "foob" any "ar")))
  ;; one part
  (should (equal (jane-aide--to-pattern-with-parts "foobar" '("oo") 0)
                 '(prefix point "f" any "oobar")))
  ;; no parts (original text was empty)
  (should (equal (jane-aide--to-pattern-with-parts "foobar" nil 0)
                 '(prefix point "foobar"))))

(ert-deftest test-aide-get-text-parts ()
  (should (equal (jane-aide--get-text-parts
                  "foar"
                  (mapcar #'jane-aide--list-to-completion
                          '(("fo" ("ob") "ar")
                            ("fo" ("xx") "ar" ("yy")))))
                 '("fo" "ar")))
  ;; unindent
  (should (equal (jane-aide--get-text-parts
                  "  foar"
                  (mapcar #'jane-aide--list-to-completion
                          '(("fo" ("ob") "ar")
                            ("fo" ("xx") "ar" ("yy")))))
                 '("fo" "ar")))
  ;; insert in the middle of leading indentation - it doesn't result in a part, since we delete
  ;; that indentation anyway.
  (should (equal (jane-aide--get-text-parts
                  "  foar"
                  (mapcar #'jane-aide--list-to-completion
                          '((" " ("zz") " fo" ("ob") "ar"))))
                 '("fo" "ar")))
  ;; one completion unindents, the other inserts indentation
  (should (equal (jane-aide--get-text-parts
                  "  foar"
                  (mapcar #'jane-aide--list-to-completion
                          '((" " ("zz") " fo" ("ob") "ar")
                            ("fo" ("xx") "ar" ("yy")))))
                 '("fo" "ar"))))

(ert-deftest test-aide-completion-caching ()
  (jane-aide--with-test-buffer
   ;; We'll give a mock completion the first time, then we expect the result to be reused
   ;; the following times while it still matches.
   (jane-aide--test-complete
    "foo" ""
    "foobar" ""
    '(("foo" ("bar"))))
   (jane-aide--test-complete
    "foobar\n    " ""
    "foobar\n    " ""
    nil)
   (jane-aide--test-complete
    "module " ""
    "module Foo\n  type t\nend" ""
    '(("module " ("Foo\n  type t\nend"))))
   (jane-aide--test-complete
    "module Foo\n  " ""
    "module Foo\n  type t\nend" ""
    nil)
   ;; Too much indentation on second line still matches
   (jane-aide--test-complete
    "module Foo\n         " ""
    "module Foo\n  type t\nend" ""
    nil)
   ;; Not enough indentation on second line still matches
   (jane-aide--test-complete
    "module Foo\n" ""
    "module Foo\n  type t\nend" ""
    nil)
   ;; Too much indentation on third line still matches
   (jane-aide--test-complete
    "module Foo\n  type t\n       " "end"
    "module Foo\n  type t\nend" ""
    nil)
   ;; Too much indentation on second line, not after a newline, does *not* match.
   (jane-aide--test-complete
    "module Foo\n  type t       " ""
    "module Foo\n  type t       " ""
    nil)
   ;;;; Completing when whitespace is the only difference
   (jane-aide--test-complete
    "foo" ""
    "foobar\n" ""
    '(("foo" ("bar\n"))))
   (jane-aide--test-complete
    "foobar\n    " ""
    "foobar\n" ""
    nil)
   ;;;; Unindenting
   (jane-aide--test-complete
    "    " ""
    "end" ""
    '((("end"))))
   ;; More spaces still matches
   (jane-aide--test-complete
    "        " ""
    "end" ""
    nil)
   ;; Fewer spaces still matches
   (jane-aide--test-complete
    " " ""
    "end" ""
    nil)
   ;; Two completions are cached
   (jane-aide--test-complete
    "let " ""
    "let " ""
    '(("let " ("x = 2"))
      ("let " ("y = 3"))))
   (jane-aide--test-complete
    "let x" ""
    "let x = 2" ""
    nil)
   (jane-aide--test-complete
    "let y" ""
    "let y = 3" ""
    nil)))

(ert-deftest test-aide-completion-merged ()
  (with-temp-buffer
    (let ((completion-styles '(basic)))
      (cl-letf (((symbol-function 'jane-ecaml-version) #'ignore)
                ((symbol-function 'jane-aide--start) #'ignore)
                ((symbol-function 'jane-aide--stop) #'ignore)
                ((symbol-function 'jane-aide--log-completion-accepted) #'ignore)
                ((symbol-function 'jane-aide--request-finished-p) #'always)
                ((symbol-function 'jane-aide--request-wait) #'identity))
        (let (tagged)
          (cl-letf
              (((symbol-function 'jane-aide--merged-append-ai-tag)
                (lambda (completion _) (push completion tagged) ""))
               ;; `jane-aide-add-ai-symbols-to-capf' assumes it's always idle completion, so force it to request.
               (jane-aide-fetch-idly t)
               (completion-at-point-functions
                (list (lambda () (jane-aide-add-ai-symbols-to-capf (list (point-min) (point-max) '("foo1" "foo2"))))))
               ((symbol-function 'jane-aide--request-start)
                (let ((completions (mapcar #'jane-aide--list-to-completion '(("foo1 bar") ("foo3 bar")))))
                  (lambda (&rest _) completions))))
            (completion-help-at-point))
          (setq tagged (sort tagged #'string<))
          (should (equal tagged '("foo1")))
          (erase-buffer))))))

;;;; Configuration
;; Elisp single-symbol configuration.
(define-advice elisp-completion-at-point (:filter-return (orig-capf) with-aide-symbols)
  (jane-aide-add-ai-symbols-to-capf orig-capf))

;; Merlin single-symbol configuration.
(define-advice merlin-completion-at-point (:filter-return (orig-capf) with-aide-symbols)
  (jane-aide-add-ai-symbols-to-capf orig-capf))

;; CR-soon sbaugh: We'd like to use `jane-aide-add-ai-symbols-to-capf' on
;; `eglot-completion-at-point' too, but Eglot has an exit-function which expects to be
;; called on one of the completions returned by Eglot.  We could wrap the exit-function,
;; but exit-function doesn't get the text properties on the completion so it's hard to
;; tell if a given completion is from AIDE or not.  So for now it's Merlin-only.

;;;; Common functionality for jane-aide-edit and jane-aide-chat
(defun jane-aide--get-visible-files ()
  "Return a list of files visited in buffers that are visible.

The return value is a list of absolute file names.

Get the absolute file paths of all buffers that are currently
open in a window on the selected frame, or any frame on the same
terminal (display)."
  (let (ret)
    (dolist (frame (frame-list))
      (when (eq (frame-terminal frame) (frame-terminal))
        (dolist (window (window-list frame))
          (when-let* ((buffer (window-buffer window))
                      (file (buffer-file-name buffer)))
            (push file ret)))))
    (delete-dups ret)))

(defconst jane-aide--max-buffer-size (* 1000 1000)
  "The maximum size of a buffer in bytes to send to AIDE.")

(defun jane-aide--get-open-files ()
  "Get all compatible open buffers to send to AIDE as context.

The return value is a list of AIDE documents that have a non-nil
file name. Each AIDE document is a value of type
`jane-aide-document'."
  (let (ret)
    (dolist (buffer (buffer-list))
      (when-let* (((buffer-file-name buffer))
                  ;; ECaml expects all strings to be utf-8. We'll assume optimistically that
                  ;; every non-raw-text buffer the user has open is utf-8 compatible. We have to
                  ;; assume this because the coding system type of .py and .el files are
                  ;; "undecided" rather than "utf-8" as .ml and .mli files are.
                  ((not (eq (coding-system-type buffer-file-coding-system) 'raw-text)))
                  ((< (buffer-size buffer) jane-aide--max-buffer-size))
                  (document (jane-aide--make-document buffer)))
        (push document ret)))
    ;; CR-someday nfarlow: Allow duplicate file paths here. Take care of deduping where
    ;; necessary in AIDE

    ;; AIDE expects the list of open files to be sorted by how recently they were accessed.
    (nreverse (cl-delete-duplicates ret :test #'string= :key #'jane-aide-document-file-path))))

(cl-defstruct jane-aide-active-file-info
  "Structure representing information about the active file.

DOCUMENT: An AIDE document as created by `jane-aide--make-document'.
OFFSET: The byte offset of the cursor in the buffer.
SELECTION: A cons cell (START . END) of byte offsets for the selected region,
            or nil if no region is active."
  (document nil :type jane-aide-document)
  (offset nil :type integer)
  (selection nil :type cons))

(defun jane-aide--get-active-file-info (buffer)
  "Get an `jane-aide-active-file-info' for BUFFER."
  (let ((document (jane-aide--make-document buffer)))
    (with-current-buffer buffer
      (make-jane-aide-active-file-info
       :document document
       :offset (jane-aide--offset-of-pos (point))
       :selection (when (use-region-p)
                    (cons
                     (jane-aide--offset-of-pos (region-beginning))
                     (jane-aide--offset-of-pos (region-end))))))))

(require 'diff-mode)
(defun jane-aide--apply-all-hunks (start end &optional reverse)
  "Apply all hunks to their respective source files.
If the prefix argument REVERSE is given, reverse-apply the hunks.
After applying all hunks, saves all buffers that were modified by a hunk."
  (save-excursion
    (goto-char start)
    (diff-hunk-next)
    (let ((applied-count 0)
          (failed-count 0)
          (already-applied-count 0)
          (modified-buffers '())
          buf pos new)
      (while (and (looking-at diff-hunk-header-re) (< (point) end))
        (pcase-let ((`(,buffer ,line-offset ,position ,_ ,new-text ,switched)
                     (diff-find-source-location nil reverse nil)))
          (setq buf buffer pos position new new-text)
          (cond
           ((and line-offset switched)
            ;; The hunk is already applied, so we count it as such
            (cl-incf already-applied-count))
           ((null line-offset)
            ;; The hunk failed to apply, increase the failed count
            (cl-incf failed-count))
           (t
            ;; Apply the hunk
            (with-current-buffer buf
              (goto-char (car pos))
              (delete-region (car pos) (cdr pos))
              (insert (car new))
              (cl-incf applied-count)
              (unless (memq buf modified-buffers)
                (push buf modified-buffers))))))
        ;; Move to the next hunk
        (diff-hunk-next))
      (dolist (buffer modified-buffers)
        (with-current-buffer buffer
          (save-buffer)))
      (message "%s %d hunks; %d failed; %d already %s."
               (if reverse "Reverted" "Applied")
               applied-count failed-count already-applied-count
               (if reverse "reverted" "applied")))))

(provide 'jane-aide)
;;; jane-aide.el ends here
