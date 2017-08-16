;;; haskell-commands.el --- Commands that can be run on the process -*- lexical-binding: t -*-

;;; Commentary:

;;; This module provides varoius `haskell-mode'
;;; specific commands such as show type signature, show info, haskell process
;;; commands and etc.

;; Copyright © 2014 Chris Done.  All rights reserved.
;;             2016 Arthur Fayzrakhmanov

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
(require 'etags)
(require 'haskell-mode)
(require 'haskell-compat)
(require 'haskell-font-lock)
(require 'haskell-string)
(require 'haskell-utils)
(require 'highlight-uses-mode)
(require 'haskell-cabal)
(require 'inf-haskell)
(require 'json)

(defcustom haskell-mode-stylish-haskell-path "stylish-haskell"
  "Path to `stylish-haskell' executable."
  :group 'haskell
  :type 'string)

(defvar url-http-response-status)
(defvar url-http-end-of-headers)
(defvar haskell-cabal-targets-history nil
  "History list for session targets.")

(defun haskell-process-hayoo-ident (ident)
  "Hayoo for IDENT, return a list of modules."
  ;; We need a real/simulated closure, because otherwise these
  ;; variables will be unbound when the url-retrieve callback is
  ;; called.
  ;; TODO: Remove when this code is converted to lexical bindings by
  ;; default (Emacs 24.1+)
  (let ((url (format haskell-process-hayoo-query-url (url-hexify-string ident))))
    (with-current-buffer (url-retrieve-synchronously url)
      (if (= 200 url-http-response-status)
          (progn
            (goto-char url-http-end-of-headers)
            (let* ((res (json-read))
                   (results (assoc-default 'result res)))
              ;; TODO: gather packages as well, and when we choose a
              ;; given import, check that we have the package in the
              ;; cabal file as well.
              (cl-mapcan (lambda (r)
                           ;; append converts from vector -> list
                           (append (assoc-default 'resultModules r) nil))
                         results)))
        (warn "HTTP error %s fetching %s" url-http-response-status url)))))

(defun haskell-process-hoogle-ident (ident)
  "Hoogle for IDENT, return a list of modules."
  (with-temp-buffer
    (let ((hoogle-error (call-process "hoogle" nil t nil "search" "--exact" ident)))
      (goto-char (point-min))
      (unless (or (/= 0 hoogle-error)
                  (looking-at "^No results found")
                  (looking-at "^package "))
        (while (re-search-forward "^\\([^ ]+\\).*$" nil t)
          (replace-match "\\1" nil nil))
        (cl-remove-if (lambda (a) (string= "" a))
                      (split-string (buffer-string)
                                    "\n"))))))

(defun haskell-process-haskell-docs-ident (ident)
  "Search with haskell-docs for IDENT, return a list of modules."
  (cl-remove-if-not
   (lambda (a) (string-match "^[[:upper:]][[:alnum:]_'.]+$" a))
   (split-string
      (with-output-to-string
        (with-current-buffer
            standard-output
          (call-process "haskell-docs"
                        nil             ; no infile
                        t               ; output to current buffer (that is string)
                        nil             ; do not redisplay
                        "--modules" ident)))
      "\n")))

;;;###autoload
(defun haskell-describe (ident)
  "Describe the given identifier IDENT."
  (interactive (list (read-from-minibuffer "Describe identifier: "
                                           (haskell-ident-at-point))))
  (let ((results (read (shell-command-to-string
                        (concat "haskell-docs --sexp "
                                ident)))))
    (help-setup-xref (list #'haskell-describe ident)
                     (called-interactively-p 'interactive))
    (save-excursion
      (with-help-window (help-buffer)
        (with-current-buffer (help-buffer)
          (if results
              (cl-loop for result in results
                       do (insert (propertize ident 'font-lock-face
                                              '((:inherit font-lock-type-face
                                                          :underline t)))
                                  " is defined in "
                                  (let ((module (cadr (assoc 'module result))))
                                    (if module
                                        (concat module " ")
                                      ""))
                                  (cadr (assoc 'package result))
                                  "\n\n")
                       do (let ((type (cadr (assoc 'type result))))
                            (when type
                              (insert (haskell-fontify-as-mode type 'haskell-mode)
                                      "\n")))
                       do (let ((args (cadr (assoc 'type results))))
                            (cl-loop for arg in args
                                     do (insert arg "\n"))
                            (insert "\n"))
                       do (insert (cadr (assoc 'documentation result)))
                       do (insert "\n\n"))
            (insert "No results for " ident)))))))

;;;###autoload
(defun haskell-rgrep (&optional prompt)
  "Grep the effective project for the symbol at point.
Very useful for codebase navigation.

Prompts for an arbitrary regexp given a prefix arg PROMPT."
  (interactive "P")
  (let ((sym (if prompt
                 (read-from-minibuffer "Look for: ")
               (haskell-ident-at-point))))
    (rgrep sym
           "*.hs *.lhs *.hsc *.chs *.hs-boot *.lhs-boot"
           inferior-haskell-root-dir)))

;;;###autoload
(defun haskell-process-do-info (&optional prompt-value)
  "Print info on the identifier at point.
If PROMPT-VALUE is non-nil, request identifier via mini-buffer."
  (interactive "P")
  (let ((at-point (haskell-ident-at-point)))
    (when (or prompt-value at-point)
      (let* ((ident (replace-regexp-in-string
                     "^!\\([A-Z_a-z]\\)"
                     "\\1"
                     (if prompt-value
                         (read-from-minibuffer "Info: " at-point)
                       at-point)))
             (modname (unless prompt-value
                        (haskell-utils-parse-import-statement-at-point)))
             (ghci-response
              (cond
               (modname (inferior-haskell-get-result
                         (format ":browse! %s" modname)))
               ((string= ident "") nil)
               (t (inferior-haskell-get-result
                   (format (if (string-match "^[a-zA-Z_]" ident)
                               ":info %s"
                             ":info (%s)")
                           (or ident
                               at-point)))))))
        (when ghci-response
          (haskell-mode-message-line ghci-response))))))

;;;###autoload
(defun haskell-mode-jump-to-def-or-tag (&optional _next-p)
  ;; FIXME NEXT-P arg is not used
  "Jump to the definition.
Jump to definition of identifier at point by consulting GHCi, or
tag table as fallback.

Remember: If GHCi is busy doing something, this will delay, but
it will always be accurate, in contrast to tags, which always
work but are not always accurate.
If the definition or tag is found, the location from which you jumped
will be pushed onto `xref--marker-ring', so you can return to that
position with `xref-pop-marker-stack'."
  (interactive "P")
  (if inferior-haskell-buffer
        (let ((initial-loc (point-marker))
            (loc (haskell-mode-find-def (haskell-ident-at-point))))
          (haskell-mode-handle-generic-loc loc)
          (unless (equal initial-loc (point-marker))
            (xref-push-marker-stack initial-loc)))
      (call-interactively 'haskell-mode-tag-find)))

;;;###autoload
(defun haskell-mode-goto-loc ()
  "Go to the location of the thing at point.
Requires the :loc-at command from GHCi."
  (interactive)
  (let ((loc (haskell-mode-loc-at)))
    (when loc
      (haskell-mode-goto-span loc))))

(defun haskell-mode-goto-span (span)
  "Jump to the SPAN, whatever file and line and column it needs to get there."
  (xref-push-marker-stack)
  (find-file (expand-file-name (plist-get span :path)
                               inferior-haskell-root-dir))
  (goto-char (point-min))
  (forward-line (1- (plist-get span :start-line)))
  (forward-char (plist-get span :start-col)))

(defun haskell-mode-find-def (ident)
  ;; TODO Check if it possible to exploit `haskell-process-do-info'
  "Find definition location of identifier IDENT.
Uses the GHCi process to find the location.  Returns nil if it
can't find the identifier or the identifier isn't a string.

Returns:

    (library <package> <module>)
    (file <path> <line> <col>)
    (module <name>)
    nil"
  (when (stringp ident)
    (let ((reply (inferior-haskell-get-result
                  (format (if (string-match "^[a-zA-Z_]" ident)
                              ":info %s"
                            ":info (%s)")
                          ident))))
      (let ((match (string-match "-- Defined \\(at\\|in\\) \\(.+\\)$" reply)))
        (when match
          (let ((defined (match-string 2 reply)))
            (let ((match (string-match "\\(.+?\\):\\([0-9]+\\):\\([0-9]+\\)$" defined)))
              (cond
               (match
                (list 'file
                      (expand-file-name (match-string 1 defined)
                                        inferior-haskell-root-dir)
                      (string-to-number (match-string 2 defined))
                      (string-to-number (match-string 3 defined))))
               (t
                (let ((match (string-match "`\\(.+?\\):\\(.+?\\)'$" defined)))
                  (if match
                      (list 'library
                            (match-string 1 defined)
                            (match-string 2 defined))
                    (let ((match (string-match "`\\(.+?\\)'$" defined)))
                      (if match
                          (list 'module
                                (match-string 1 defined)))))))))))))))

;;;###autoload
(defun haskell-mode-jump-to-def (ident)
  "Jump to definition of identifier IDENT at point."
  (interactive
   (list
    (haskell-string-drop-qualifier
     (haskell-ident-at-point))))
  (let ((loc (haskell-mode-find-def ident)))
    (when loc
      (haskell-mode-handle-generic-loc loc))))

(defun haskell-mode-handle-generic-loc (loc)
  "Either jump to or echo a generic location LOC.
Either a file or a library."
  (cl-case (car loc)
    (file (progn
              (find-file (elt loc 1))
              (goto-char (point-min))
              (forward-line (1- (elt loc 2)))
              (goto-char (+ (line-beginning-position)
                            (1- (elt loc 3))))))
    (library (message "Defined in `%s' (%s)."
                      (elt loc 2)
                      (elt loc 1)))
    (module (message "Defined in `%s'."
                     (elt loc 1)))))

(defun haskell-mode-loc-at ()
  "Get the location at point.
Requires the :loc-at command from GHCi."
  (let ((pos (or (when (region-active-p)
                   (cons (region-beginning)
                         (region-end)))
                 (haskell-spanable-pos-at-point)
                 (cons (point)
                       (point)))))
    (when pos
      (let ((reply (inferior-haskell-get-result
                    (save-excursion
                      (format ":loc-at %s %d %d %d %d %s"
                              (buffer-file-name)
                              (progn (goto-char (car pos))
                                     (line-number-at-pos))
                              (1+ (current-column)) ;; GHC uses 1-based columns.
                              (progn (goto-char (cdr pos))
                                     (line-number-at-pos))
                              (1+ (current-column)) ;; GHC uses 1-based columns.
                              (buffer-substring-no-properties (car pos)
                                                              (cdr pos)))))))
        (if reply
            (if (string-match "\\(.*?\\):(\\([0-9]+\\),\\([0-9]+\\))-(\\([0-9]+\\),\\([0-9]+\\))"
                              reply)
                (list :path (match-string 1 reply)
                      :start-line (string-to-number (match-string 2 reply))
                      ;; ;; GHC uses 1-based columns.
                      :start-col (1- (string-to-number (match-string 3 reply)))
                      :end-line (string-to-number (match-string 4 reply))
                      ;; GHC uses 1-based columns.
                      :end-col (1- (string-to-number (match-string 5 reply))))
              (error (propertize reply 'face 'compilation-error)))
          (error (propertize "No reply. Is :loc-at supported?"
                             'face 'compilation-error)))))))

;;;###autoload
(defun haskell-mode-stylish-buffer ()
  "Apply stylish-haskell to the current buffer.

Use `haskell-mode-stylish-haskell-path' to know where to find
stylish-haskell executable.  This function tries to preserve
cursor position and markers by using
`haskell-mode-buffer-apply-command'."
  (interactive)
  (haskell-mode-buffer-apply-command haskell-mode-stylish-haskell-path))

(defun haskell-mode-buffer-apply-command (cmd)
  "Execute shell command CMD with current buffer as input and output.
Use buffer as input and replace the whole buffer with the
output.  If CMD fails the buffer remains unchanged."
  (set-buffer-modified-p t)
  (let* ((out-file (make-temp-file "stylish-output"))
         (err-file (make-temp-file "stylish-error")))
        (unwind-protect
          (let* ((_errcode
                  (call-process-region (point-min) (point-max) cmd nil
                                       `((:file ,out-file) ,err-file)
                                       nil))
                 (err-file-empty-p
                  (equal 0 (nth 7 (file-attributes err-file))))
                 (out-file-empty-p
                  (equal 0 (nth 7 (file-attributes out-file)))))
            (if err-file-empty-p
                (if out-file-empty-p
                    (message "Error: %s produced no output and no error information, leaving buffer alone" cmd)
                  ;; Command successful, insert file with replacement to preserve
                  ;; markers.
                  (insert-file-contents out-file nil nil nil t))
              (progn
                ;; non-null stderr, command must have failed
                (message "Error: %s ended with errors, leaving buffer alone" cmd)
                (with-temp-buffer
                  (insert-file-contents err-file)
                  ;; use (warning-minimum-level :debug) to see this
                  (display-warning cmd
                                   (buffer-substring-no-properties (point-min) (point-max))
                                   :debug)))))
          (ignore-errors
            (delete-file err-file))
          (ignore-errors
            (delete-file out-file)))))

;;;###autoload
(defun haskell-mode-find-uses ()
  "Find use cases of the identifier at point and highlight them all."
  (interactive)
  (let ((spans (haskell-mode-uses-at)))
    (unless (null spans)
      (highlight-uses-mode 1)
      (cl-loop for span in spans
               do (haskell-mode-make-use-highlight span)))))

(defun haskell-mode-make-use-highlight (span)
  "Make a highlight overlay at the given SPAN."
  (save-window-excursion
    (save-excursion
      (haskell-mode-goto-span span)
      (save-excursion
        (highlight-uses-mode-highlight
         (progn
           (goto-char (point-min))
           (forward-line (1- (plist-get span :start-line)))
           (forward-char (plist-get span :start-col))
           (point))
         (progn
           (goto-char (point-min))
           (forward-line (1- (plist-get span :end-line)))
           (forward-char (plist-get span :end-col))
           (point)))))))

(defun haskell-mode-uses-at ()
  "Get the locations of use cases for the ident at point.
Requires the :uses command from GHCi.
Requires :set +c to be set"
  (let ((pos (or (when (region-active-p)
                   (cons (region-beginning)
                         (region-end)))
                 (haskell-ident-pos-at-point)
                 (cons (point)
                       (point)))))
    (haskell-set+c)
    (when pos
      (let ((reply (inferior-haskell-get-result
                    (save-excursion
                      (format ":uses %s %d %d %d %d %s"
                              (buffer-file-name)
                              (progn (goto-char (car pos))
                                     (line-number-at-pos))
                              (1+ (current-column)) ;; GHC uses 1-based columns.
                              (progn (goto-char (cdr pos))
                                     (line-number-at-pos))
                              (1+ (current-column)) ;; GHC uses 1-based columns.
                              (buffer-substring-no-properties (car pos)
                                                              (cdr pos)))))))
        (if reply
            (let ((lines (split-string reply "\n" t)))
              (cl-remove-if
               #'null
               (mapcar (lambda (line)
                         (if (string-match "\\(.*?\\):(\\([0-9]+\\),\\([0-9]+\\))-(\\([0-9]+\\),\\([0-9]+\\))"
                                           line)
                             (list :path (match-string 1 line)
                                   :start-line (string-to-number (match-string 2 line))
                                   ;; ;; GHC uses 1-based columns.
                                   :start-col (1- (string-to-number (match-string 3 line)))
                                   :end-line (string-to-number (match-string 4 line))
                                   ;; GHC uses 1-based columns.
                                   :end-col (1- (string-to-number (match-string 5 line))))
                           (error (propertize line 'face 'compilation-error))))
                       lines)))
          (error (propertize "No reply. Is :uses supported?"
                             'face 'compilation-error)))))))

(provide 'haskell-commands)
;;; haskell-commands.el ends here
