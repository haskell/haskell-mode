;; haskell.el --- Top-level Haskell package -*- lexical-binding: t -*-

;; Copyright © 2014 Chris Done.  All rights reserved.
;;             2016 Arthur Fayzrakhmanov
;;             2017 Vasantha Ganesh Kanniappan <vasanthaganesh.k@tuta.io>

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

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'haskell-hoogle)
(require 'haskell-commands)
(require 'haskell-string)
(require 'haskell-completions)
(require 'haskell-utils)
(require 'haskell-customize)
(require 'haskell-compile)

(defvar haskell-set+c-p nil
  "t if `:set +c` else nil")

(defun haskell-set+c ()
  "set `:set +c` is not already set"
  (if (not haskell-set+c-p)
      (inferior-haskell-get-result ":set +c")))

(defvar interactive-haskell-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-l") 'haskell-process-load-file)
    (define-key map (kbd "C-c C-r") 'haskell-process-load-file)
    (define-key map (kbd "C-c C-i") 'haskell-process-do-info)
    (define-key map (kbd "M-.")     'haskell-mode-jump-to-def-or-tag)
    (define-key map (kbd "C-c C-c") 'haskell-process-cabal-build)
    (define-key map (kbd "C-c v c") 'haskell-cabal-visit-file)
    (define-key map (kbd "C-c C-x") 'haskell-process-cabal)
    (define-key map (kbd "C-c C-b") 'switch-to-haskell)
    (define-key map (kbd "C-c C-z") 'switch-to-haskell)
    map)
  "Keymap for using `interactive-haskell-mode'.")

;;;###autoload
(define-minor-mode interactive-haskell-mode
  "Minor mode for enabling haskell-process interaction."
  :lighter " Interactive"
  :keymap interactive-haskell-mode-map
  (add-hook 'completion-at-point-functions
            #'haskell-completions-sync-repl-completion-at-point
            nil
            t))

(defvar xref-prompt-for-identifier nil)

;;;###autoload
(defun haskell-mode-jump-to-tag (&optional next-p)
  "Jump to the tag of the given identifier.

Give optional NEXT-P parameter to override value of
`xref-prompt-for-identifier' during definition search."
  (interactive "P")
  (let ((ident (haskell-string-drop-qualifier (haskell-ident-at-point)))
        (tags-file-dir (haskell-cabal--find-tags-dir))
        (tags-revert-without-query t))
    (when (and ident
               (not (string= "" (haskell-string-trim ident)))
               tags-file-dir)
      (let ((tags-file-name (concat tags-file-dir "TAGS")))
        (cond ((file-exists-p tags-file-name)
               (let ((xref-prompt-for-identifier next-p))
                 (xref-find-definitions ident)))
              (t (haskell-mode-generate-tags ident)))))))

;;;###autoload
(defun haskell-mode-after-save-handler ()
  "Function that will be called after buffer's saving."
  (when haskell-tags-on-save
    (ignore-errors (haskell-mode-generate-tags))))

;;;###autoload
(defun haskell-mode-tag-find (&optional _next-p)
  "The tag find function, specific for the particular session."
  (interactive "P")
  (cond
   ((elt (syntax-ppss) 3) ;; Inside a string
    (haskell-mode-jump-to-filename-in-string))
   (t (call-interactively 'haskell-mode-jump-to-tag))))

(defun haskell-mode-jump-to-filename-in-string ()
  "Jump to the filename in the current string."
  (let* ((string (save-excursion
                   (buffer-substring-no-properties
                    (1+ (search-backward-regexp "\"" (line-beginning-position) nil 1))
                    (1- (progn (forward-char 1)
                               (search-forward-regexp "\"" (line-end-position) nil 1))))))
         (fp (expand-file-name string
                               inferior-haskell-root-dir)))
    (find-file
     (read-file-name
      ""
      fp
      fp))))

(defvar interactive-haskell-loaded-files (haskell-make-set))

;;;###autoload
(defun haskell-process-load-file ()
  "Load or reload the file in current buffer with the help of `:load' or
`:reload' functionality that comes with ghci.
Errors that might arise are put in the `*haskell-compilation*' buffer."
  (interactive)
  (save-some-buffers (not compilation-ask-about-save)
                     compilation-save-buffers-predicate)
  (let ((filename (buffer-file-name)))
    (cond ((haskell-in-set-p interactive-haskell-loaded-files filename)
           (haskell-compile-load (inferior-haskell-get-result ":reload!"))
           (message (format "Reloaded %s" filename)))
          (t
           (haskell-compile-load (inferior-haskell-get-result (format ":load \"%s\"" filename)))
           (haskell-add-to-set interactive-haskell-loaded-files filename)
           (message (format "Loaded %s" filename))))))

(defun haskell-compile-error-p ()
  "Return t if an error (ghci's) is found in current buffer."
  (search-forward-regexp "^\\(\\(?:[A-Z]:\\)?[^ \r\n:][^\r\n:]*\\):\\([0-9()-:]+\\):?"
                         nil
                         (lambda () nil)
                         1))

(defun haskell-compile-load (haskell-load-traceback)
  "A `*haskell-compilation*' buffer is created if it does not exist,
then the traceback from GHCi is displayed."
  (pop-to-buffer "*haskell-compilation*")
  (with-current-buffer "*haskell-compilation*"
    (setq inhibit-read-only t)
    (erase-buffer)
    (insert haskell-load-traceback)
    (haskell-compilation-mode)
    (save-excursion
      (goto-char (point-min))
      (cond ((haskell-compile-error-p) (compilation-handle-exit 'exit 1 "failed"))
            (t (compilation-handle-exit 'exit 0 "finished"))))))

;;;###autoload
(defun haskell-process-cabal-build ()
  "Build the Cabal project."
  (interactive)
  (haskell-process-do-cabal "build"))

;;;###autoload
(defun haskell-process-cabal (p)
  "Prompts for a Cabal command to run.
Argument P is the command to run."
  (interactive "P")
  (if p
      (haskell-process-do-cabal
       (read-from-minibuffer "Cabal command (e.g. install): "))
    (haskell-process-do-cabal
     (funcall haskell-completing-read-function "Cabal command: "
              (append haskell-cabal-commands
                      (list "build --ghc-options=-fforce-recomp"))))))

(defun haskell-process-do-cabal (command)
  "Run a Cabal COMMAND."
  (shell-command (concat "cabal " command)
                 (get-buffer-create "*haskell-process-log*")
                 (get-buffer-create "*haskell-process-log*"))
  (switch-to-buffer-other-window (get-buffer "*haskell-process-log*")))

(provide 'haskell)
;;; haskell.el ends here
