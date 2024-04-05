;; -*- lexical-binding: t; -*-

(defun codeium-get ()
  "sends request to codeium, return (reqbody . resbody) or nil
if user input is encountered, schedule a `CancelRequest' and return nil

this uses `sit-for', which means that timers can be ran while this function
waits, but these function called by timers must exit before this function
returns. Prefer using `codeium-request' directly instead.
"
  (let* ((tracker (codeium-state-alive-tracker codeium-state))
	 (requestid (cl-incf (codeium-state-last-request-id codeium-state)))
	 (_ (puthash requestid t (codeium-state-pending-table codeium-state)))
	 (reqbody
	  (progn
	    (unless (codeium-state-alive-tracker codeium-state)
	      (error "codeium-state is not alive! %s" codeium-state))
	    (let ((body
		   (let (body)
		     (push (cons 'codeium/editor_options/insert_spaces (if indent-tabs-mode :false t)) body)
		     ;; https://www.reddit.com/r/emacs/comments/5b7o9r/elisp_how_to_concat_newline_into_string_regarding/
		     (push (cons 'codeium/document/line_ending "\n") body)
		     (push
		      (cons 'codeium/document/language
			    (let ((mode major-mode))
			      (while (not (alist-get mode codeium-language-alist))
				(setq mode (get mode 'derived-mode-parent)))
			      (alist-get mode codeium-language-alist)))
		      body)
		     (push (cons 'codeium/document/editor_language (symbol-name major-mode)) body)
		     (push (cons 'codeium/document/cursor_offset
				 (codeium-utf8-byte-length
				  (buffer-substring-no-properties (point-min) (point))))
			   body)
		     (push (cons 'codeium/document/text (buffer-string)) body)
		     (push (cons 'codeium/metadata/api_key
				 (if-let ((api-key (or (codeium-state-last-api-key codeium-state) (codeium-get-saved-api-key))))
				     (setq codeium/metadata/api_key api-key)
				   (setq codeium/metadata/api_key
					 (lambda (_api codeium-state)
					   (when-let ((api-key (codeium-state-last-api-key codeium-state)))
					     (setq codeium/metadata/api_key api-key))))
				   nil))
			   body)
		     (push (cons 'codeium/metadata/request_id (cl-incf codeium-global-requestid-counter)) body)
		     (push (cons 'codeium/metadata/ide_version emacs-version) body)
		     (push (cons 'codeium/metadata/ide_name "emacs") body)
		     (push (cons 'codeium/metadata/extension_version codeium-local-server-version) body)
		     body)))
	      (codeium-request-with-body 'GetCompletions codeium-state body (codeium-state-alive-tracker codeium-state)
					 (lambda (res)
					   (when (gethash requestid (codeium-state-pending-table codeium-state))
					     (remhash requestid (codeium-state-pending-table codeium-state))
					     (puthash requestid res (codeium-state-results-table codeium-state)))))
	      body)))
	 (rst 'noexist))
    (while (and (eq tracker (codeium-state-alive-tracker codeium-state)) (eq rst 'noexist) (not (input-pending-p)))
      (sit-for (codeium-get-config 'codeium-delay nil codeium-state))
      (setq rst (gethash requestid (codeium-state-results-table codeium-state) 'noexist)))
    (when (and (eq rst 'noexist) (eq tracker (codeium-state-alive-tracker codeium-state)))
      (remhash requestid (codeium-state-results-table codeium-state)))
    (unless (or (eq rst 'error)
		(eq rst 'noexist))
      (cons reqbody rst))))
