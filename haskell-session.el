;;; haskell-session.el -- Haskell sessions.

;; Copyright (C) 2011-2012 Chris Done

;; Author: Chris Done <chrisdone@gmail.com>

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

(require 'haskell-cabal)
(require 'haskell-string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Globals

(defvar haskell-sessions (list)
  "All Haskell sessions in the Emacs session.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finding/clearing the session

(defun haskell-session-maybe ()
  "Maybe get the Haskell session, return nil if there isn't one."
  (if (default-boundp 'haskell-session)
      haskell-session
    (setq haskell-session nil)))

;;;###autoload
(defun haskell-session ()
  "Get the Haskell session, prompt if there isn't one or fail."
  (or (haskell-session-maybe)
      (haskell-session-assign
       (or (haskell-session-from-buffer)
           (haskell-session-new-assume-from-cabal)
           (haskell-session-choose)
           (haskell-session-new)))))

(defun haskell-session-new-assume-from-cabal ()
  "Prompt to create a new project based on a guess from the nearest Cabal file."
  (when (y-or-n-p (format "Start a new project named “%s”? "
                          (haskell-session-default-name)))
    (haskell-session-make (haskell-session-default-name))))

(defun haskell-session-from-buffer ()
  "Get the session based on the buffer."
  (let ((sessions (remove-if-not (lambda (session) 
                                   (haskell-is-prefix-of (file-name-directory (buffer-file-name))
                                                         (haskell-session-cabal-dir session)))
                                 haskell-sessions)))
    (sort sessions (lambda (a b) (< (length (haskell-session-cabal-dir a))
                                    (length (haskell-session-cabal-dir b)))))
    (when (consp sessions)
      (car sessions))))

(defun haskell-session-new ()
  "Make a new session."
  (let ((name (read-from-minibuffer "Project name: " (haskell-session-default-name))))
    (when (not (string= name ""))
      (haskell-session-make name))))

(defun haskell-session-default-name ()
  "Generate a default project name for the new project prompt."
  (let ((file (haskell-cabal-find-file)))
    (when file
      (downcase (file-name-sans-extension
                 (file-name-nondirectory file))))))

(defun haskell-session-assign (session)
  "Set the current session."
  (set (make-local-variable 'haskell-session) session))

(defun haskell-session-choose ()
  "Find a session by choosing from a list of the current sessions."
  (when haskell-sessions
    (let* ((session-name (ido-completing-read
                          "Choose Haskell session: "
                          (mapcar 'haskell-session-name haskell-sessions)))
           (session (find-if (lambda (session)
                               (string= (haskell-session-name session)
                                        session-name))
                             haskell-sessions)))
      session)))

(defun haskell-session-clear ()
  "Clear the buffer of any Haskell session choice."
  (set (make-local-variable 'haskell-session) nil))

(defun haskell-session-clear-all ()
  "Clear the buffer of any Haskell session choice."
  (haskell-session-clear)
  (setq haskell-sessions nil))

(defun haskell-session-change ()
  "Change the session for the current buffer."
  (interactive)
  (haskell-session-clear)
  (haskell-session))

(defun haskell-session-strip-dir (session file)
  "Strip the load dir from the file path."
  (let ((cur-dir (haskell-session-current-dir session)))
    (if (> (length file) (length cur-dir))
        (if (string= (substring file 0 (length cur-dir))
                     cur-dir)
            (replace-regexp-in-string 
             "^[/\\]" ""
             (substring file 
                        (length cur-dir)))
          file)
      file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Building the session

(defun haskell-session-make (name)
  "Make a Haskell session."
  (let ((session (set (make-local-variable 'haskell-session)
                      (list (cons 'name name)))))
    (add-to-list 'haskell-sessions session)
    (haskell-process-start session)
    session))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Accessing the session

(defun haskell-session-name (s)
  "Get the session name."
  (haskell-session-get s 'name))

(defun haskell-session-interactive-buffer (s)
  "Get the session interactive buffer."
  (let ((buffer (haskell-session-get s 'interactive-buffer)))
    (if (and buffer (buffer-live-p buffer))
        buffer
      (let ((buffer (get-buffer-create (format "*%s*" (haskell-session-name s)))))
        (haskell-session-set-interactive-buffer s buffer)
        (with-current-buffer buffer
          (haskell-interactive-mode s))
        (switch-to-buffer-other-window buffer)
        buffer))))

(defun haskell-session-set-interactive-buffer (s v)
  "Set the session interactive buffer."
  (haskell-session-set s 'interactive-buffer v))

(defun haskell-session-set-process (s v)
  "Set the session process."
  (haskell-session-set s 'process v))

(defun haskell-session-process (s)
  "Get the session process."
  (haskell-session-get s 'process))

(defun haskell-session-set-cabal-dir (s v)
  "Set the session cabal-dir."
  (haskell-session-set s 'cabal-dir v))

(defun haskell-session-set-current-dir (s v)
  "Set the session current directory."
  (haskell-session-set s 'current-dir v))

(defun haskell-session-current-dir (s)
  "Get the session current directory."
  (let ((dir (haskell-session-get s 'current-dir)))
    (or dir
        (haskell-process-cd t))))

(defun haskell-session-cabal-dir (s)
  "Get the session cabal-dir."
  (let ((dir (haskell-session-get s 'cabal-dir)))
    (if dir
        dir
      (let ((set-dir (haskell-cabal-get-dir)))
        (if set-dir
            (progn (haskell-session-set-cabal-dir s set-dir)
                   set-dir)
          (haskell-session-cabal-dir s))))))

(defun haskell-session-get (s key)
  "Get the session `key`."
  (let ((x (assoc key s)))
    (when x
      (cdr x))))

(defun haskell-session-set (s key value) 
  "Set the session's `key`."
  (delete-if (lambda (prop) (equal (car prop) key)) s)
  (setf (cdr s) (cons (cons key value)
                      (cdr s))))

(provide 'haskell-session)
