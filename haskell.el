;;; haskell.el --- Top-level Haskell package

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

(require 'haskell-mode)
(require 'haskell-process)
(require 'haskell-debug)
(require 'haskell-interactive-mode)
(require 'haskell-repl)
(require 'haskell-load)
(require 'haskell-commands)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic configuration hooks

(add-hook 'haskell-process-ended-hook 'haskell-process-prompt-restart)
(add-hook 'kill-buffer-hook 'haskell-interactive-kill)

(defun haskell-interactive-mode-return ()
  "Handle the return key."
  (interactive)
  (cond
   ((haskell-interactive-at-compile-message)
    (next-error-internal))
   (t
    (haskell-interactive-handle-expr))))

(defun haskell-session-kill (&optional leave-interactive-buffer)
  "Kill the session process and buffer, delete the session.
0. Prompt to kill all associated buffers.
1. Kill the process.
2. Kill the interactive buffer.
3. Walk through all the related buffers and set their haskell-session to nil.
4. Remove the session from the sessions list."
  (interactive)
  (let* ((session (haskell-session))
         (name (haskell-session-name session))
         (also-kill-buffers
          (and haskell-ask-also-kill-buffers
               (y-or-n-p (format "Killing `%s'. Also kill all associated buffers?" name)))))
    (haskell-kill-session-process session)
    (unless leave-interactive-buffer
      (kill-buffer (haskell-session-interactive-buffer session)))
    (cl-loop for buffer in (buffer-list)
             do (with-current-buffer buffer
                  (when (and (boundp 'haskell-session)
                             (string= (haskell-session-name haskell-session) name))
                    (setq haskell-session nil)
                    (when also-kill-buffers
                      (kill-buffer)))))
    (setq haskell-sessions
          (cl-remove-if (lambda (session)
                          (string= (haskell-session-name session)
                                   name))
                        haskell-sessions))))

(defun haskell-interactive-kill ()
  "Kill the buffer and (maybe) the session."
  (interactive)
  (when (eq major-mode 'haskell-interactive-mode)
    (when (and (boundp 'haskell-session)
               haskell-session
               (y-or-n-p "Kill the whole session?"))
      (haskell-session-kill t))))

(defun haskell-session-make (name)
  "Make a Haskell session."
  (when (haskell-session-lookup name)
    (error "Session of name %s already exists!" name))
  (let ((session (setq haskell-session
                       (list (cons 'name name)))))
    (add-to-list 'haskell-sessions session)
    (haskell-process-start session)
    session))

(defun haskell-session-new-assume-from-cabal ()
  "Prompt to create a new project based on a guess from the nearest Cabal file."
  (let ((name (haskell-session-default-name)))
    (unless (haskell-session-lookup name)
      (when (y-or-n-p (format "Start a new project named “%s”? "
                              name))
        (haskell-session-make name)))))

;;;###autoload
(defun haskell-session ()
  "Get the Haskell session, prompt if there isn't one or fail."
  (or (haskell-session-maybe)
      (haskell-session-assign
       (or (haskell-session-from-buffer)
           (haskell-session-new-assume-from-cabal)
           (haskell-session-choose)
           (haskell-session-new)))))

;;;###autoload
(defun haskell-interactive-switch ()
  "Switch to the interactive mode for this session."
  (interactive)
  (let ((initial-buffer (current-buffer))
        (buffer (haskell-session-interactive-buffer (haskell-session))))
    (with-current-buffer buffer
      (setq haskell-interactive-previous-buffer initial-buffer))
    (unless (eq buffer (window-buffer))
      (switch-to-buffer-other-window buffer))))

(defun haskell-session-new ()
  "Make a new session."
  (let ((name (read-from-minibuffer "Project name: " (haskell-session-default-name))))
    (when (not (string= name ""))
      (let ((session (haskell-session-lookup name)))
        (if session
            (when (y-or-n-p (format "Session %s already exists. Use it?" name))
              session)
          (haskell-session-make name))))))

;;;###autoload
(defun haskell-session-change ()
  "Change the session for the current buffer."
  (interactive)
  (haskell-session-assign (or (haskell-session-new-assume-from-cabal)
                              (haskell-session-choose)
                              (haskell-session-new))))

(defun haskell-process-prompt-restart (process)
  "Prompt to restart the died process."
  (let ((process-name (haskell-process-name process)))
    (if haskell-process-suggest-restart
        (cl-case (read-event
                  (propertize (format "The Haskell process `%s' has died. Restart? (y, n, l: show process log)"
                                      process-name)
                              'face 'minibuffer-prompt))
          (?y (haskell-process-start (haskell-process-session process)))
          (?l (let* ((response (haskell-process-response process))
                     (buffer (get-buffer "*haskell-process-log*")))
                (if buffer
                    (switch-to-buffer buffer)
                  (progn (switch-to-buffer (get-buffer-create "*haskell-process-log*"))
                         (insert response)))))
          (?n))
      (message (format "The Haskell process `%s' is dearly departed."
                       process-name)))))

(defun haskell-process ()
  "Get the current process from the current session."
  (haskell-session-process (haskell-session)))

(defun haskell-interactive-buffer ()
  "Get the interactive buffer of the session."
  (haskell-session-interactive-buffer (haskell-session)))

(defun haskell-kill-session-process (&optional session)
  "Kill the process."
  (interactive)
  (let* ((session (or session (haskell-session)))
         (existing-process (get-process (haskell-session-name session))))
    (when (processp existing-process)
      (haskell-interactive-mode-echo session "Killing process ...")
      (haskell-process-set (haskell-session-process session) 'is-restarting t)
      (delete-process existing-process))))

(defun haskell-interactive-mode-visit-error ()
  "Visit the buffer of the current (or last) error message."
  (interactive)
  (with-current-buffer (haskell-session-interactive-buffer (haskell-session))
    (if (progn (goto-char (line-beginning-position))
               (looking-at haskell-interactive-mode-error-regexp))
        (progn (forward-line -1)
               (haskell-interactive-jump-to-error-line))
      (progn (goto-char (point-max))
             (haskell-interactive-mode-error-backward)
             (haskell-interactive-jump-to-error-line)))))

(defun haskell-mode-contextual-space ()
  "Contextually do clever stuff when hitting space."
  (interactive)
  (if (or (not (bound-and-true-p interactive-haskell-mode))
          (not (haskell-session-maybe)))
      (self-insert-command 1)
    (cond ((and haskell-mode-contextual-import-completion
                (save-excursion (forward-word -1)
                                (looking-at "^import$")))
           (insert " ")
           (let ((module (haskell-complete-module-read
                          "Module: "
                          (haskell-session-all-modules (haskell-session)))))
             (insert module)
             (haskell-mode-format-imports)))
          ((not (string= "" (save-excursion (forward-char -1) (haskell-ident-at-point))))
           (let ((ident (save-excursion (forward-char -1) (haskell-ident-at-point))))
             (insert " ")
             (haskell-process-do-try-info ident)))
          (t (insert " ")))))

(defun haskell-mode-jump-to-tag (&optional next-p)
  "Jump to the tag of the given identifier."
  (interactive "P")
  (let ((ident (haskell-ident-at-point))
        (tags-file-name (haskell-session-tags-filename (haskell-session)))
        (tags-revert-without-query t))
    (when (not (string= "" (haskell-trim ident)))
      (cond ((file-exists-p tags-file-name)
             (find-tag ident next-p))
            (t (haskell-process-generate-tags ident))))))

(defun haskell-mode-after-save-handler ()
  "Function that will be called after buffer's saving."
  (when haskell-tags-on-save
    (ignore-errors (when (and (boundp 'haskell-session) haskell-session)
                     (haskell-process-generate-tags))))
  (when haskell-stylish-on-save
    (ignore-errors (haskell-mode-stylish-buffer))
    (let ((before-save-hook '())
          (after-save-hook '()))
      (basic-save-buffer))))

(defun haskell-mode-tag-find (&optional next-p)
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
                               (haskell-session-cabal-dir (haskell-session)))))
    (find-file
     (read-file-name
      ""
      fp
      fp))))

(provide 'haskell)
