;;; haskell-interactive-mode.el --- The interactive Haskell mode

;; Copyright (C) 2011-2012  Chris Done

;; Author: Chris Done <chrisdone@gmail.com>

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Todo:

;;; Code:

(require 'haskell-compile)
(require 'haskell-navigate-imports)
(require 'haskell-process)
(require 'haskell-collapse)
(require 'haskell-session)
(require 'haskell-show)

(require 'ansi-color)
(require 'cl-lib)
(require 'etags)

(defvar haskell-interactive-mode-history-index)
(make-variable-buffer-local 'haskell-interactive-mode-history-index)

(defvar haskell-interactive-mode-history (list))
(make-variable-buffer-local 'haskell-interactive-mode-history)

(defvar haskell-interactive-mode-completion-cache)
(make-variable-buffer-local 'haskell-interactive-mode-completion-cache)

(defvar haskell-interactive-mode-old-prompt-start
  nil
  "Mark used for the old beginning of the prompt.")
(make-variable-buffer-local 'haskell-interactive-mode-old-prompt-start)

(defun haskell-interactive-prompt-regex ()
  "Generate a regex for searching for any occurence of the prompt
at the beginning of the line. This should prevent any
interference with prompts that look like haskell expressions."
  (concat "^" (regexp-quote haskell-interactive-prompt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Globals used internally


(define-derived-mode haskell-interactive-mode fundamental-mode "Interactive-Haskell"
  "Interactive mode for Haskell.

See Info node `(haskell-mode)haskell-interactive-mode' for more
information.

Key bindings:
\\{haskell-interactive-mode-map}"
  :group 'haskell-interactive
  (setq haskell-interactive-mode-history (list))
  (setq haskell-interactive-mode-history-index 0)
  (setq haskell-interactive-mode-completion-cache nil)

  (setq next-error-function 'haskell-interactive-next-error-function)
  (add-hook 'completion-at-point-functions
            'haskell-interactive-mode-completion-at-point-function nil t)

  (haskell-interactive-mode-prompt))

(defvar haskell-interactive-mode-prompt-start
  nil
  "Mark used for the beginning of the prompt.")

(defvar haskell-interactive-mode-result-end
  nil
  "Mark used to figure out where the end of the current result
  output is. Used to distinguish betwen user input.")

(defvar haskell-interactive-previous-buffer nil
  "Records the buffer to which `haskell-interactive-switch-back' should jump.
This is set by `haskell-interactive-switch', and should otherwise
be nil.")
(make-variable-buffer-local 'haskell-interactive-previous-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hooks


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Faces

(defface haskell-interactive-face-prompt
  '((t :inherit font-lock-function-name-face))
  "Face for the prompt."
  :group 'haskell-interactive)

(defface haskell-interactive-face-compile-error
  '((t :inherit compilation-error))
  "Face for compile errors."
  :group 'haskell-interactive)

(defface haskell-interactive-face-compile-warning
  '((t :inherit compilation-warning))
  "Face for compiler warnings."
  :group 'haskell-interactive)

(defface haskell-interactive-face-result
  '((t :inherit font-lock-string-face))
  "Face for the result."
  :group 'haskell-interactive)

(defface haskell-interactive-face-garbage
  '((t :inherit font-lock-string-face))
  "Face for trailing garbage after a command has completed."
  :group 'haskell-interactive)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Actions

(defun haskell-interactive-mode-newline-indent ()
  "Make newline and indent."
  (interactive)
  (newline)
  (indent-according-to-mode))

(defun haskell-interactive-mode-kill-whole-line ()
  "Kill the whole REPL line."
  (interactive)
  (kill-region haskell-interactive-mode-prompt-start
               (line-end-position)))

(defun haskell-interactive-switch-back ()
  "Switch back to the buffer from which this interactive buffer was reached."
  (interactive)
  (if haskell-interactive-previous-buffer
      (switch-to-buffer-other-window haskell-interactive-previous-buffer)
    (message "No previous buffer.")))

(defun haskell-interactive-mode-space (n)
  "Handle the space key."
  (interactive "p")
  (if (and (bound-and-true-p god-local-mode)
           (fboundp 'god-mode-self-insert))
      (call-interactively 'god-mode-self-insert)
    (if (haskell-interactive-at-compile-message)
        (next-error-no-select 0)
      (self-insert-command n))))

(defun haskell-interactive-at-prompt ()
  "If at prompt, returns start position of user-input, otherwise returns nil."
  (if (>= (point)
          haskell-interactive-mode-prompt-start)
      haskell-interactive-mode-prompt-start
    nil))

(define-derived-mode haskell-error-mode
  special-mode "Error"
  "Major mode for viewing Haskell compile errors.")

;; (define-key haskell-error-mode-map (kbd "q") 'quit-window)

(defun haskell-interactive-mode-handle-h (&optional bound)
  "Handle ^H in output."
  (let ((bound (point-min))
        (inhibit-read-only t))
    (save-excursion
      (while (search-backward "\b" bound t 1)
        (save-excursion
          (forward-char)
          (let ((end (point)))
            (if (search-backward-regexp "[^\b]" bound t 1)
                (forward-char)
              (goto-char (point-min)))
            (let ((start (point)))
              (delete-region (max (- (point) (- end start))
                                  (point-min))
                             end))))))))

(defun haskell-interactive-mode-cleanup-response (expr response)
  "Ignore the mess that GHCi outputs on multi-line input."
  (if (not (string-match "\n" expr))
      response
    (let ((i 0)
          (out "")
          (lines (length (split-string expr "\n"))))
      (cl-loop for part in (split-string response "| ")
               do (setq out
                        (concat out
                                (if (> i lines)
                                    (concat (if (or (= i 0) (= i (1+ lines))) "" "| ") part)
                                  "")))
               do (setq i (1+ i)))
      out)))

(defun haskell-interactive-mode-multi-line (expr)
  "If a multi-line expression has been entered, then reformat it to be:

:{
do the
   multi-liner
   expr
:}
"
  (if (not (string-match "\n" expr))
      expr
    (let* ((i 0)
           (lines (split-string expr "\n"))
           (len (length lines))
           (indent (make-string (length haskell-interactive-prompt)
                                ? )))
      (mapconcat 'identity
                 (cl-loop for line in lines
                          collect (cond ((= i 0)
                                         (concat ":{" "\n" line))
                                        ((= i (1- len))
                                         (concat line "\n" ":}"))
                                        (t
                                         line))
                          do (setq i (1+ i)))
                 "\n"))))

(defun haskell-interactive-trim (line)
  "Trim indentation off of lines in the REPL."
  (if (and (string-match "^[ ]+" line)
           (> (length line)
              (length haskell-interactive-prompt)))
      (substring line
                 (length haskell-interactive-prompt))
    line))

(defun haskell-interactive-mode-line-is-query (line)
  "Is LINE actually a :t/:k/:i?"
  (and (string-match "^:[itk] " line)
       t))

(defun haskell-interactive-mode-beginning ()
  "Go to the start of the line."
  (interactive)
  (if (haskell-interactive-at-prompt)
      (goto-char haskell-interactive-mode-prompt-start)
    (move-beginning-of-line nil)))

(defun haskell-interactive-mode-input-partial ()
  "Get the interactive mode input up to point."
  (let ((input-start (haskell-interactive-at-prompt)))
    (unless input-start
      (error "not at prompt"))
    (buffer-substring-no-properties input-start (point))))

(defun haskell-interactive-mode-input ()
  "Get the interactive mode input."
  (buffer-substring-no-properties
   haskell-interactive-mode-prompt-start
   (point-max)))

(defun haskell-interactive-mode-prompt (&optional session)
  "Show a prompt at the end of the REPL buffer.
If SESSION is non-nil, use the REPL buffer associated with
SESSION, otherwise operate on the current buffer.
"
  (with-current-buffer (if session
                           (haskell-session-interactive-buffer session)
                         (current-buffer))
    (goto-char (point-max))
    (insert (propertize haskell-interactive-prompt
                        'font-lock-face 'haskell-interactive-face-prompt
                        'read-only t
                        'rear-nonsticky t
                        'prompt t))
    (let ((marker (set (make-local-variable 'haskell-interactive-mode-prompt-start)
                       (make-marker))))
      (set-marker marker
                  (point)
                  (current-buffer))
      (when nil
        (let ((o (make-overlay (point) (point-max) nil nil t)))
          (overlay-put o 'line-prefix (make-string (length haskell-interactive-prompt)
                                                   ? )))))
    (when haskell-interactive-mode-scroll-to-bottom
      (haskell-interactive-mode-scroll-to-bottom))))

(defun haskell-interactive-mode-eval-result (session text)
  "Insert the result of an eval as plain text."
  (with-current-buffer (haskell-session-interactive-buffer session)
    (goto-char (point-max))
    (let ((start (point)))
      (insert (ansi-color-apply
               (propertize text
                           'font-lock-face 'haskell-interactive-face-result
                           'rear-nonsticky t
                           'read-only t
                           'prompt t
                           'result t)))
      (haskell-interactive-mode-handle-h start))
    (let ((marker (set (make-local-variable 'haskell-interactive-mode-result-end)
                       (make-marker))))
      (set-marker marker
                  (point)
                  (current-buffer)))
    (when haskell-interactive-mode-scroll-to-bottom
      (haskell-interactive-mode-scroll-to-bottom))))

(defun haskell-interactive-mode-scroll-to-bottom ()
  "Scroll to bottom."
  (let ((w (get-buffer-window (current-buffer))))
    (when w
      (goto-char (point-max))
      (set-window-point w (point-max)))))

(defun haskell-interactive-mode-compile-error (session message)
  "Echo an error."
  (haskell-interactive-mode-compile-message
   session message 'haskell-interactive-face-compile-error))

(defun haskell-interactive-mode-compile-warning (session message)
  "Warning message."
  (haskell-interactive-mode-compile-message
   session message 'haskell-interactive-face-compile-warning))

(defun haskell-interactive-mode-compile-message (session message type)
  "Echo a compiler warning."
  (with-current-buffer (haskell-session-interactive-buffer session)
    (setq next-error-last-buffer (current-buffer))
    (save-excursion
      (haskell-interactive-mode-goto-end-point)
      (let ((lines (string-match "^\\(.*\\)\n\\([[:unibyte:][:nonascii:]]+\\)" message)))
        (when lines
          (insert (propertize (concat (match-string 1 message) " …\n")
                              'font-lock-face type
                              'read-only t
                              'rear-nonsticky t
                              'expandable t))
          (insert (propertize (concat (match-string 2 message) "\n")
                              'font-lock-face type
                              'read-only t
                              'rear-nonsticky t
                              'collapsible t
                              'invisible haskell-interactive-mode-hide-multi-line-errors
                              'message-length (length (match-string 2 message)))))
        (unless lines
          (insert (propertize (concat message "\n")
                              'font-lock-face type
                              'read-only t
                              'rear-nonsticky t)))))))

(defun haskell-interactive-mode-insert (session message)
  "Echo a read only piece of text before the prompt."
  (with-current-buffer (haskell-session-interactive-buffer session)
    (save-excursion
      (haskell-interactive-mode-goto-end-point)
      (insert (propertize message
                          'read-only t
                          'rear-nonsticky t)))))

(defun haskell-interactive-mode-goto-end-point ()
  "Go to the 'end' of the buffer (before the prompt.)"
  (goto-char haskell-interactive-mode-prompt-start)
  (goto-char (line-beginning-position)))

(defun haskell-interactive-mode-history-add (input)
  "Add item to the history."
  (setq haskell-interactive-mode-history
        (cons ""
              (cons input
                    (cl-remove-if (lambda (i) (or (string= i input) (string= i "")))
                                  haskell-interactive-mode-history))))
  (setq haskell-interactive-mode-history-index
        0))

(defun haskell-mode-message-line (str)
  "Message only one line, multiple lines just disturbs the programmer."
  (let ((lines (split-string str "\n" t)))
    (when (and (car lines) (stringp (car lines)))
      (message "%s"
               (concat (car lines)
                       (if (and (cdr lines) (stringp (cadr lines)))
                           (format " [ %s .. ]" (haskell-string-take (haskell-trim (cadr lines)) 10))
                         ""))))))

(defun haskell-interactive-mode-tab ()
  "Do completion if at prompt or else try collapse/expand."
  (interactive)
  (cond
   ((haskell-interactive-at-prompt)
    (completion-at-point))
   ((get-text-property (point) 'collapsible)
    (let ((column (current-column)))
      (search-backward-regexp "^[^ ]")
      (haskell-interactive-mode-tab-expand)
      (goto-char (+ column (line-beginning-position)))))
   (t (haskell-interactive-mode-tab-expand))))

(defun haskell-interactive-mode-tab-expand ()
  "Expand the rest of the message."
  (cond ((get-text-property (point) 'expandable)
         (let* ((pos (1+ (line-end-position)))
                (visibility (get-text-property pos 'invisible))
                (length (1+ (get-text-property pos 'message-length))))
           (let ((inhibit-read-only t))
             (put-text-property pos
                                (+ pos length)
                                'invisible
                                (not visibility)))))))

(defconst haskell-interactive-mode-error-regexp
  "^\\([A-Z]?:?[^\r\n:]+\\):\\([0-9()-:]+\\):?")

(defun haskell-interactive-at-compile-message ()
  "Am I on a compile message?"
  (and (not (haskell-interactive-at-prompt))
       (save-excursion
         (goto-char (line-beginning-position))
         (looking-at haskell-interactive-mode-error-regexp))))

(defun haskell-interactive-mode-error-backward (&optional count)
  "Go backward to the previous error."
  (interactive)
  (search-backward-regexp haskell-interactive-mode-error-regexp nil t count))

(defun haskell-interactive-mode-error-forward (&optional count)
  "Go forward to the next error, or return to the REPL."
  (interactive)
  (goto-char (line-end-position))
  (if (search-forward-regexp haskell-interactive-mode-error-regexp nil t count)
      (progn (goto-char (line-beginning-position))
             t)
    (progn (goto-char (point-max))
           nil)))

(defun haskell-interactive-mode-delete-compile-messages (session &optional file-name)
  "Delete compile messages in REPL buffer.
If FILE-NAME is non-nil, restrict to removing messages concerning
FILE-NAME only."
  (with-current-buffer (haskell-session-interactive-buffer session)
    (save-excursion
      (goto-char (point-min))
      (when (search-forward-regexp "^Compilation failed.$" nil t 1)
        (let ((inhibit-read-only t))
          (delete-region (line-beginning-position)
                         (1+ (line-end-position))))
        (goto-char (point-min)))
      (while (when (re-search-forward haskell-interactive-mode-error-regexp nil t)
               (let ((msg-file-name (match-string-no-properties 1))
                     (msg-startpos (line-beginning-position)))
                 ;; skip over hanging continuation message lines
                 (while (progn (forward-line) (looking-at "^[ ]+")))

                 (when (or (not file-name) (string= file-name msg-file-name))
                   (let ((inhibit-read-only t))
                     (set-text-properties msg-startpos (point) nil))
                   (delete-region msg-startpos (point))
                   ))
               t)))))

;;;###autoload
(defun haskell-interactive-mode-reset-error (session)
  "Reset the error cursor position."
  (interactive)
  (with-current-buffer (haskell-session-interactive-buffer session)
    (haskell-interactive-mode-goto-end-point)
    (let ((mrk (point-marker)))
      (haskell-session-set session 'next-error-locus nil)
      (haskell-session-set session 'next-error-region (cons mrk (copy-marker mrk t))))
    (goto-char (point-max))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc

(defun haskell-session-interactive-buffer (s)
  "Get the session interactive buffer."
  (let ((buffer (haskell-session-get s 'interactive-buffer)))
    (if (and buffer (buffer-live-p buffer))
        buffer
      (let ((buffer (get-buffer-create (format "*%s*" (haskell-session-name s)))))
        (haskell-session-set-interactive-buffer s buffer)
        (with-current-buffer buffer
          (haskell-interactive-mode)
          (haskell-session-assign s))
        (switch-to-buffer-other-window buffer)
        buffer))))

(defun haskell-process-cabal-live (state buffer)
  "Do live updates for Cabal processes."
  (haskell-interactive-mode-insert
   (haskell-process-session (cadr state))
   (replace-regexp-in-string
    haskell-process-prompt-regex
    ""
    (substring buffer (cl-cadddr state))))
  (setf (cl-cdddr state) (list (length buffer)))
  nil)

(defun haskell-process-parse-error (string)
  "Parse the line number from the error."
  (let ((span nil))
    (cl-loop for regex
             in haskell-compilation-error-regexp-alist
             do (when (string-match (car regex) string)
                  (setq span
                        (list :file (match-string 1 string)
                              :line (string-to-number (match-string 2 string))
                              :col (string-to-number (match-string 4 string))
                              :line2 (when (match-string 3 string)
                                       (string-to-number (match-string 3 string)))
                              :col2 (when (match-string 5 string)
                                      (string-to-number (match-string 5 string)))))))
    span))

(defun haskell-process-suggest-add-package (session msg)
  "Add the (matched) module to your cabal file."
  (let* ((suggested-package (match-string 1 msg))
         (package-name (replace-regexp-in-string "-[^-]+$" "" suggested-package))
         (version (progn (string-match "\\([^-]+\\)$" suggested-package)
                         (match-string 1 suggested-package)))
         (cabal-file (concat (haskell-session-name session)
                             ".cabal")))
    (when (y-or-n-p
           (format "Add `%s' to %s?"
                   package-name
                   cabal-file))
      (haskell-cabal-add-dependency package-name version nil t))))

(defun haskell-process-suggest-remove-import (session file import line)
  "Suggest removing or commenting out IMPORT on LINE."
  (let ((continue t)
        (first t))
    (cl-case (read-event
              (propertize (format "%sThe import line `%s' is redundant. Remove? (y, n, c: comment out)  "
                                  (if (not first)
                                      "Please answer n, y or c: "
                                    "")
                                  import)
                          'face 'minibuffer-prompt))
      (?y
       (haskell-process-find-file session file)
       (save-excursion
         (goto-char (point-min))
         (forward-line (1- line))
         (goto-char (line-beginning-position))
         (delete-region (line-beginning-position)
                        (line-end-position))))
      (?n
       (message "Ignoring redundant import %s" import))
      (?c
       (haskell-process-find-file session file)
       (save-excursion
         (goto-char (point-min))
         (forward-line (1- line))
         (goto-char (line-beginning-position))
         (insert "-- "))))))

(defun haskell-process-suggest-pragma (session pragma extension file)
  "Suggest to add something to the top of the file."
  (let ((string  (format "{-# %s %s #-}" pragma extension)))
    (when (y-or-n-p (format "Add %s to the top of the file? " string))
      (haskell-process-find-file session file)
      (save-excursion
        (goto-char (point-min))
        (insert (concat string "\n"))))))

(provide 'haskell-interactive-mode)

;;; haskell-interactive-mode.el ends here
