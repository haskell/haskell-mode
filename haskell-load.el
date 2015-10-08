;;; haskell-load.el --- Compiling and loading modules in the GHCi process -*- lexical-binding: t -*-

;; Copyright (c) 2014 Chris Done. All rights reserved.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'cl-lib)
(require 'haskell-process)
(require 'haskell-interactive-mode)
(require 'haskell-modules)
(require 'haskell-commands)
(require 'haskell-session)

(defun haskell-session-look-config-changes (session)
  "Checks whether a cabal configuration file has
changed. Restarts the process if that is the case."
  (let ((current-checksum (haskell-session-get session 'cabal-checksum))
        (new-checksum (haskell-cabal-compute-checksum
                       (haskell-session-get session 'cabal-dir))))
    (when (not (string= current-checksum new-checksum))
      (haskell-interactive-mode-echo session (format "Cabal file changed: %s" new-checksum))
      (haskell-session-set-cabal-checksum session
                                          (haskell-session-get session 'cabal-dir))
      (unless (and haskell-session-prompt-restart-on-cabal-change
                   (not (y-or-n-p "Cabal file changed; restart GHCi process? ")))
        (haskell-session-start (haskell-interactive-session))))))

(defun haskell-session-live-build (process buffer echo-in-repl)
  "Show live updates for loading files."
  (cond ((haskell-session-consume
          process
          (concat "\\[[ ]*\\([0-9]+\\) of \\([0-9]+\\)\\]"
                  " Compiling \\([^ ]+\\)[ ]+"
                  "( \\([^ ]+\\), \\([^ ]+\\) )[^\r\n]*[\r\n]+"))
         (haskell-session-echo-load-message process buffer echo-in-repl nil)
         t)
        ((haskell-session-consume
          process
          (concat "\\[[ ]*\\([0-9]+\\) of \\([0-9]+\\)\\]"
                  " Compiling \\[TH\\] \\([^ ]+\\)[ ]+"
                  "( \\([^ ]+\\), \\([^ ]+\\) )[^\r\n]*[\r\n]+"))
         (haskell-session-echo-load-message process buffer echo-in-repl t)
         t)
        ((haskell-session-consume process "Loading package \\([^ ]+\\) ... linking ... done.\n")
         (haskell-mode-message-line
          (format "Loading: %s"
                  (match-string 1 buffer)))
         t)
        ((haskell-session-consume
          process
          "^Preprocessing executables for \\(.+?\\)\\.\\.\\.")
         (let ((msg (format "Preprocessing: %s" (match-string 1 buffer))))
           (haskell-interactive-mode-echo
            process
            msg)
           (haskell-mode-message-line msg)))
        ((haskell-session-consume process "Linking \\(.+?\\) \\.\\.\\.")
         (let ((msg (format "Linking: %s" (match-string 1 buffer))))
           (haskell-interactive-mode-echo process msg)
           (haskell-mode-message-line msg)))
        ((haskell-session-consume process "\nBuilding \\(.+?\\)\\.\\.\\.")
         (let ((msg (format "Building: %s" (match-string 1 buffer))))
           (haskell-interactive-mode-echo
            process
            msg)
           (haskell-mode-message-line msg)))))

(defun haskell-session-load-complete (session process buffer reload module-buffer &optional cont)
  "Handle the complete loading response. BUFFER is the string of
text being sent over the process pipe. MODULE-BUFFER is the
actual Emacs buffer of the module being loaded."
  (when (get-buffer (format "*%s:splices*" (haskell-session-name session)))
    (with-current-buffer (haskell-interactive-mode-splices-buffer session)
      (erase-buffer)))
  (let* ((ok (cond ((haskell-session-consume process "Ok, modules loaded: \\(.+\\)\\.$")       t)
		   ((haskell-session-consume process "Failed, modules loaded: \\(.+\\)\\.$") nil)
		   (t (error (message "Unexpected response from haskell process.")))))
	 (modules (haskell-session-extract-modules buffer))
	 (cursor (haskell-session-response-cursor process))
	 (warning-count 0))
    (haskell-session-set-response-cursor process 0)
    (haskell-check-remove-overlays module-buffer)
    (while (haskell-session-errors-warnings module-buffer session process buffer)
      (setq warning-count (1+ warning-count)))
    (haskell-session-set-response-cursor process cursor)
    (if (and (not reload)
	     haskell-session-reload-with-fbytecode)
	(haskell-session-reload-with-fbytecode process module-buffer)
	(haskell-session-import-modules process (car modules)))
    (if ok
	(haskell-mode-message-line (if reload "Reloaded OK." "OK."))
	(haskell-interactive-mode-compile-error session "Compilation failed."))
    (when cont
      (condition-case e
	  (funcall cont ok)
	(error (message "%S" e))
	(quit nil)))))

(defun haskell-session-suggest-imports (session file modules ident)
  "Given a list of MODULES, suggest adding them to the import section."
  (cl-assert session)
  (cl-assert file)
  (cl-assert ident)
  (let* ((suggested-already (haskell-session-suggested-imports session))
         (module (cond ((> (length modules) 1)
                        (when (y-or-n-p (format "Identifier `%s' not in scope, choose module to import?"
                                                ident))
                          (haskell-complete-module-read "Module: " modules)))
                       ((= (length modules) 1)
                        (let ((module (car modules)))
                          (unless (member module suggested-already)
                            (haskell-session-set-suggested-imports session (cons module suggested-already))
                            (when (y-or-n-p (format "Identifier `%s' not in scope, import `%s'?"
                                                    ident
                                                    module))
                              module)))))))
    (when module
      (haskell-session-find-file session file)
      (haskell-add-import module))))

(defun haskell-session-trigger-suggestions (session msg file line)
  "Trigger prompting to add any extension suggestions."
  (cond ((let ((case-fold-search nil))
           (or (and (string-match " -X\\([A-Z][A-Za-z]+\\)" msg)
                    (not (string-match "\\([A-Z][A-Za-z]+\\) is deprecated" msg)))
               (string-match "Use \\([A-Z][A-Za-z]+\\) to permit this" msg)
               (string-match "Use \\([A-Z][A-Za-z]+\\) to allow" msg)
               (string-match "use \\([A-Z][A-Za-z]+\\)" msg)
               (string-match "You need \\([A-Z][A-Za-z]+\\)" msg)))
         (when haskell-session-suggest-language-pragmas
           (haskell-session-suggest-pragma session "LANGUAGE" (match-string 1 msg) file)))
        ((string-match " The \\(qualified \\)?import of[ ][‘`‛]\\([^ ]+\\)['’] is redundant" msg)
         (when haskell-session-suggest-remove-import-lines
           (haskell-session-suggest-remove-import session
                                                  file
                                                  (match-string 2 msg)
                                                  line)))
        ((string-match "Warning: orphan instance: " msg)
         (when haskell-session-suggest-no-warn-orphans
           (haskell-session-suggest-pragma session "OPTIONS" "-fno-warn-orphans" file)))
        ((or (string-match "against inferred type [‘`‛]\\[Char\\]['’]" msg)
             (string-match "with actual type [‘`‛]\\[Char\\]['’]" msg))
         (when haskell-session-suggest-overloaded-strings
           (haskell-session-suggest-pragma session "LANGUAGE" "OverloadedStrings" file)))
        ((string-match "^Not in scope: .*[‘`‛]\\(.+\\)['’]$" msg)
         (let* ((match1 (match-string 1 msg))
                (ident (if (string-match "^[A-Za-z0-9_'.]+\\.\\(.+\\)$" match1)
                           ;; Skip qualification.
                           (match-string 1 match1)
                         match1)))
           (when haskell-session-suggest-hoogle-imports
             (let ((modules (haskell-session-hoogle-ident ident)))
               (haskell-session-suggest-imports session file modules ident)))
           (when haskell-session-suggest-haskell-docs-imports
             (let ((modules (haskell-session-haskell-docs-ident ident)))
               (haskell-session-suggest-imports session file modules ident)))
           (when haskell-session-suggest-hayoo-imports
             (let ((modules (haskell-session-hayoo-ident ident)))
               (haskell-session-suggest-imports session file modules ident)))))
        ((string-match "^[ ]+It is a member of the hidden package [‘`‛]\\([^@\r\n]+\\).*['’].$" msg)
         (when haskell-session-suggest-add-package
           (haskell-session-suggest-add-package session msg)))))

(defun haskell-session-do-cabal (command)
  "Run a Cabal command."
  (let ((process (haskell-interactive-session)))
    (cond
     ((let ((child (haskell-session-process process)))
        (not (equal 'run (process-status child))))
      (message "Process is not running, so running directly.")
      (shell-command (concat "cabal " command)
                     (get-buffer-create "*haskell-session-log*")
                     (get-buffer-create "*haskell-session-log*"))
      (switch-to-buffer-other-window (get-buffer "*haskell-session-log*")))
     (t (haskell-session-queue-command
         process
         (make-haskell-command
          :state (list (haskell-interactive-session) process command 0)

          :go
          (lambda (state)
            (haskell-session-send-string
             (cadr state)
             (format haskell-session-do-cabal-format-string
                     (haskell-session-cabal-dir (car state))
                     (format "%s %s"
                             (cl-ecase (haskell-session-type)
                               ('ghci haskell-session-path-cabal)
                               ('cabal-repl haskell-session-path-cabal)
                               ('cabal-ghci haskell-session-path-cabal)
                               ('stack-ghci haskell-session-path-stack))
                             (cl-caddr state)))))

          :live
          (lambda (state buffer)
            (let ((cmd (replace-regexp-in-string "^\\([a-z]+\\).*"
                                                 "\\1"
                                                 (cl-caddr state))))
              (cond ((or (string= cmd "build")
                         (string= cmd "install"))
                     (haskell-session-live-build (cadr state) buffer t))
                    (t
                     (haskell-session-cabal-live state buffer)))))

          :complete
          (lambda (state response)
            (let* ((process (cadr state))
                   (session process)
                   (message-count 0)
                   (cursor (haskell-session-response-cursor process)))
	      ;; XXX: what the hell about the rampant code duplication?
              (haskell-session-set-response-cursor process 0)
              (while (haskell-session-errors-warnings nil session process response)
                (setq message-count (1+ message-count)))
              (haskell-session-set-response-cursor process cursor)
              (let ((msg (format "Complete: cabal %s (%s compiler messages)"
                                 (cl-caddr state)
                                 message-count)))
                (haskell-interactive-mode-echo session msg)
                (when (= message-count 0)
                  (haskell-interactive-mode-echo
                   session
                   "No compiler messages, dumping complete output:")
                  (haskell-interactive-mode-echo session response))
                (haskell-mode-message-line msg)
                (when (and haskell-notify-p
                           (fboundp 'notifications-notify))
                  (notifications-notify
                   :title (format "*%s*" (haskell-session-name (car state)))
                   :body msg
                   :app-name (cl-ecase (haskell-session-type)
                               ('ghci haskell-session-path-cabal)
                               ('cabal-repl haskell-session-path-cabal)
                               ('cabal-ghci haskell-session-path-cabal)
                               ('stack-ghci haskell-session-path-stack))
                   :app-icon haskell-session-logo)))))))))))

(defun haskell-session-echo-load-message (process buffer echo-in-repl th)
  "Echo a load message."
  (let ((session process)
        (module-name (match-string 3 buffer))
        (file-name (match-string 4 buffer)))
    (haskell-interactive-show-load-message
     session
     'compiling
     module-name
     (haskell-session-strip-dir session file-name)
     echo-in-repl
     th)))

(defun haskell-session-extract-modules (buffer)
  "Extract the modules from the process buffer."
  (let* ((modules-string (match-string 1 buffer))
         (modules (split-string modules-string ", ")))
    (cons modules modules-string)))

;;;###autoload
(defface haskell-error-face
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "#dc322f"))
    (t
     :inherit error))
  "Face used for marking error lines."
  :group 'haskell-mode)

;;;###autoload
(defface haskell-warning-face
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "#b58900"))
    (t
     :inherit warning))
  "Face used for marking warning lines."
  :group 'haskell-mode)

;;;###autoload
(defface haskell-hole-face
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "#6c71c4"))
    (t
     :inherit warning))
  "Face used for marking hole lines."
  :group 'haskell-mode)

(defvar haskell-check-error-fringe   (propertize "!" 'display '(left-fringe exclamation-mark)))
(defvar haskell-check-warning-fringe (propertize "?" 'display '(left-fringe question-mark)))
(defvar haskell-check-hole-fringe    (propertize "_" 'display '(left-fringe horizontal-bar)))

(defun haskell-check-overlay-p (ovl)
  (overlay-get ovl 'haskell-check))

(defun haskell-check-filter-overlays (xs)
  (cl-remove-if-not 'haskell-check-overlay-p xs))

(defun haskell-check-remove-overlays (buffer)
  (with-current-buffer buffer
    (remove-overlays (point-min) (point-max) 'haskell-check t)))

(defmacro with-overlay-properties (proplist ovl &rest body)
  "Evaluate BODY with names in PROPLIST bound to the values of
correspondingly-named overlay properties of OVL."
  (let ((ovlvar (cl-gensym "OVL-")))
    `(let* ((,ovlvar ,ovl)
	    ,@(mapcar (lambda (p) `(,p (overlay-get ,ovlvar ',p))) proplist))
       ,@body)))

(defun overlay-start> (o1 o2)
  (> (overlay-start o1) (overlay-start o2)))
(defun overlay-start< (o1 o2)
  (< (overlay-start o1) (overlay-start o2)))

(defun first-overlay-in-if (test beg end)
  (let ((ovls (cl-remove-if-not test (overlays-in beg end))))
    (cl-first (sort (cl-copy-list ovls) 'overlay-start<))))

(defun last-overlay-in-if (test beg end)
  (let ((ovls (cl-remove-if-not test (overlays-in beg end))))
    (cl-first (sort (cl-copy-list ovls) 'overlay-start>))))

(defun haskell-error-overlay-briefly (ovl)
  (with-overlay-properties (haskell-msg haskell-msg-type) ovl
    (cond ((not (eq haskell-msg-type 'warning))
	   haskell-msg)
	  ((string-prefix-p "Warning:\n    " haskell-msg)
	   (cl-subseq haskell-msg 13))
	  (t (error "Invariant failed: a warning message from GHC has unexpected form: %s." haskell-msg)))))

(defun haskell-goto-error-overlay (ovl)
  (cond (ovl
	 (goto-char (overlay-start ovl))
	 (haskell-mode-message-line (haskell-error-overlay-briefly ovl)))
	(t
	 (message "No further notes from Haskell compiler."))))

(defun haskell-goto-prev-error ()
  (interactive)
  (haskell-goto-error-overlay
   (let ((ovl-at (cl-first (haskell-check-filter-overlays (overlays-at (point))))))
     (or (last-overlay-in-if 'haskell-check-overlay-p
			     (point-min) (if ovl-at (overlay-start ovl-at) (point)))
	 ovl-at))))

(defun haskell-goto-next-error ()
  (interactive)
  (haskell-goto-error-overlay
   (let ((ovl-at (cl-first (haskell-check-filter-overlays (overlays-at (point))))))
     (or (first-overlay-in-if 'haskell-check-overlay-p
			      (if ovl-at (overlay-end ovl-at) (point)) (point-max))
	 ovl-at))))

(defun haskell-check-paint-overlay (buffer error-from-this-file-p line msg file type hole coln)
  (with-current-buffer buffer
    (let (beg end)
      (goto-char (point-min))
      ;; XXX: we can avoid excess buffer walking by relying on the maybe-fact that
      ;;      GHC sorts error messages by line number, maybe.
      (cond
	(error-from-this-file-p
	 (forward-line (1- line))
	 (forward-char (1- coln))
	 (setq beg (point))
	 (if (eq type 'hole)
	     (forward-char (length hole))
	     (skip-chars-forward "^[:space:]" (line-end-position)))
	 (setq end (point)))
	(t
	 (setq beg (point))
	 (forward-line)
	 (setq end (point))))
      (let ((ovl (make-overlay beg end)))
	(overlay-put ovl 'haskell-check t)
	(overlay-put ovl 'haskell-file file)
	(overlay-put ovl 'haskell-msg msg)
	(overlay-put ovl 'haskell-msg-type type)
	(overlay-put ovl 'help-echo msg)
	(overlay-put ovl 'haskell-hole hole)
	(cl-destructuring-bind (face fringe) (cl-case type
					       (warning (list 'haskell-warning-face haskell-check-warning-fringe))
					       (hole    (list 'haskell-hole-face    haskell-check-hole-fringe))
					       (error   (list 'haskell-error-face   haskell-check-error-fringe)))
	  (overlay-put ovl 'before-string fringe)
	  (overlay-put ovl 'face face))))))

(defun haskell-session-errors-warnings (module-buffer session process buffer &optional return-only)
  "Trigger handling type errors or warnings.  Either prints the
messages in the interactive buffer or if CONT is specified,
passes the error onto that.

When MODULE-BUFFER is non-NIL, paint error overlays."
  (save-excursion
    (cond
      ((haskell-session-consume
	process
	"\\(Module imports form a cycle:[ \n]+module [^ ]+ ([^)]+)[[:unibyte:][:nonascii:]]+?\\)\nFailed")
       (let ((err (match-string 1 buffer)))
	 (if (string-match "module [`'‘‛]\\([^ ]+\\)['’`] (\\([^)]+\\))" err)
	     (let* ((default-directory (haskell-session-current-dir session))
		    (module (match-string 1 err))
		    (file (match-string 2 err))
		    (relative-file-name (file-relative-name file)))
	       (unless return-only
		 (haskell-interactive-show-load-message
		  session
		  'import-cycle
		  module
		  relative-file-name
		  nil
		  nil)
		 (haskell-interactive-mode-compile-error
		  session
		  (format "%s:1:0: %s"
			  relative-file-name
			  err)))
	       (list :file file :line 1 :col 0 :msg err :type 'error))
	     t)))
      ((haskell-session-consume
	process
	(concat "[\r\n]\\([A-Z]?:?[^ \r\n:][^:\n\r]+\\):\\([0-9()-:]+\\):"
		"[ \n\r]+\\([[:unibyte:][:nonascii:]]+?\\)\n[^ ]"))
       (haskell-session-set-response-cursor process
					    (- (haskell-session-response-cursor process) 1))
       (let* ((buffer (haskell-session-response process))
	      (file (match-string 1 buffer))
	      (location-raw (match-string 2 buffer))
	      (error-msg (match-string 3 buffer))
	      (type (cond ((string-match "^Warning:" error-msg)  'warning)
			  ((string-match "^Splicing " error-msg) 'splice)
			  (t                                     'error)))
	      (critical (not (eq type 'warning)))
	      ;; XXX: extract hole information, pass down to `haskell-check-paint-overlay'
	      (final-msg (format "%s:%s: %s"
				 (haskell-session-strip-dir session file)
				 location-raw
				 error-msg))
	      (location (haskell-session-parse-error (concat file ":" location-raw ": x")))
	      (line (plist-get location :line))
	      (col1 (plist-get location :col)))
	 (when module-buffer
	   (haskell-check-paint-overlay module-buffer (string= (file-truename (buffer-file-name module-buffer)) (file-truename file))
					line error-msg file type nil col1))
	 (if return-only
	     (list :file file :line line :col col1 :msg error-msg :type type)
	     (progn (funcall (cl-case type
			       (warning  'haskell-interactive-mode-compile-warning)
			       (splice   'haskell-interactive-mode-compile-splice)
			       (error    'haskell-interactive-mode-compile-error))
			     session final-msg)
		    (when critical
		      (haskell-mode-message-line final-msg))
		    (haskell-session-trigger-suggestions
		     session
		     error-msg
		     file
		     (plist-get (haskell-session-parse-error final-msg) :line))
		    t)))))))

(defun haskell-interactive-show-load-message (session type module-name file-name echo th)
  "Show the '(Compiling|Loading) X' message."
  (let ((msg (concat
              (cl-ecase type
                ('compiling
                 (if haskell-interactive-mode-include-file-name
                     (format "Compiling: %s (%s)" module-name file-name)
                   (format "Compiling: %s" module-name)))
                ('loading (format "Loading: %s" module-name))
                ('import-cycle (format "Module has an import cycle: %s" module-name)))
              (if th " [TH]" ""))))
    (haskell-mode-message-line msg)
    (when haskell-interactive-mode-delete-superseded-errors
      (haskell-interactive-mode-delete-compile-messages session file-name))
    (when echo
      (haskell-interactive-mode-echo session msg))))

;;;###autoload
(defun haskell-session-reload-devel-main ()
  "Reload the module `DevelMain' and then run
`DevelMain.update'. This is for doing live update of the code of
servers or GUI applications. Put your development version of the
program in `DevelMain', and define `update' to auto-start the
program on a new thread, and use the `foreign-store' package to
access the running context across :load/:reloads in GHCi."
  (interactive)
  (with-current-buffer (or (get-buffer "DevelMain.hs")
                           (if (y-or-n-p "You need to open a buffer named DevelMain.hs. Find now?")
                               (ido-find-file)
                             (error "No DevelMain.hs buffer.")))
    (let ((session (haskell-interactive-session)))
      (let ((process (haskell-interactive-session)))
        (haskell-session-queue-command
         process
         (make-haskell-command
          :state (list :session session
                       :process process
                       :buffer (current-buffer))
          :go (lambda (state)
                (haskell-session-send-string (plist-get state ':process)
                                             ":l DevelMain"))
          :live (lambda (state buffer)
                  (haskell-session-live-build (plist-get state ':process)
                                              buffer
                                              nil))
          :complete (lambda (state response)
                      (haskell-session-load-complete
                       (plist-get state ':session)
                       (plist-get state ':process)
                       response
                       nil
                       (plist-get state ':buffer)
                       (lambda (ok)
                         (when ok
                           (haskell-session-queue-without-filters
                            (haskell-interactive-session)
                            "DevelMain.update")
                           (message "DevelMain updated.")))))))))))

(provide 'haskell-load)
