;;; haskell-session.el --- Haskell sessions

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

(require 'cl-lib)
(require 'haskell-cabal)
(require 'haskell-string)
(require 'haskell-customize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Session management basis: global state, bookkeeping, lookup, process creation

;; Used internally
(defvar haskell-session)

(make-variable-buffer-local 'haskell-session)

(defvar haskell-sessions (list)
  "All Haskell sessions in the Emacs session.")

(defun haskell-session-assign (session)
  "Assign current buffer to SESSION.
More verbose doc string for `haskell-session-assign`
This could be helpfull for temporal or auxilar buffers such as
presentation mode buffers (e.g. in case when session is killed
with all relevant buffers)."
  (set (make-local-variable 'haskell-session) session))

(defun haskell-session-default-name ()
  "Generate a default project name for the new project prompt."
  (let ((file (haskell-cabal-find-file)))
    (or (when file
          (downcase (file-name-sans-extension
                     (file-name-nondirectory file))))
        "haskell")))

(defun haskell-session-lookup (name)
  "Get the session by name."
  (cl-remove-if-not (lambda (s)
                      (string= name (haskell-session-name s)))
                    haskell-sessions))

;; depends-on:
;;  - haskell-process.el:haskell-process-start
(defun haskell-session-make (name)
  "Make a Haskell session."
  (when (haskell-session-lookup name)
    (error "Session of name %s already exists!" name))
  (let ((session (setq haskell-session
                       (list (cons 'name name)))))
    (add-to-list 'haskell-sessions session)
    (haskell-process-start session)
    session))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entry points: Tier 1 -- callees of Tier 0, see below

;;;###autoload
(defun haskell-session-maybe ()
  "Maybe get the Haskell session, return nil if there isn't one."
  (if (default-boundp 'haskell-session)
      haskell-session
    (setq haskell-session nil)))

(defun haskell-session-from-buffer ()
  "Get the session based on the buffer."
  (when (and (buffer-file-name)
             (consp haskell-sessions))
    (cl-reduce (lambda (acc a)
                 (let ((dir (haskell-session-cabal-dir a t)))
                   (if dir
                       (if (string-prefix-p dir
					    (file-name-directory (buffer-file-name)))
                           (if acc
                               (if (and
                                    (> (length (haskell-session-cabal-dir a t))
                                       (length (haskell-session-cabal-dir acc t))))
                                   a
                                 acc)
                             a)
                         acc)
                     acc)))
               haskell-sessions
               :initial-value nil)))

(defun haskell-session-new-assume-from-cabal ()
  "Prompt to create a new project based on a guess from the nearest Cabal file.
If `haskell-process-load-or-reload-prompt' is nil, accept `default'."
  (let ((name (haskell-session-default-name)))
    (unless (haskell-session-lookup name)
      (if (or (not haskell-process-load-or-reload-prompt)
	      (y-or-n-p (format "Start a new project named “%s”? " name)))
	    (haskell-session-make name)))))

(defun haskell-session-choose ()
  "Find a session by choosing from a list of the current sessions."
  (when haskell-sessions
    (let* ((session-name (funcall haskell-completing-read-function
                                  "Choose Haskell session: "
                                  (cl-remove-if (lambda (name)
                                                  (and haskell-session
                                                       (string= (haskell-session-name haskell-session)
                                                                name)))
                                                (mapcar 'haskell-session-name haskell-sessions))))
           (session (cl-find-if (lambda (session)
                                  (string= (haskell-session-name session)
                                           session-name))
                                haskell-sessions)))
      session)))

(defun haskell-session-new ()
  "Make a new session."
  (let ((name (read-from-minibuffer "Project name: " (haskell-session-default-name))))
    (when (not (string= name ""))
      (let ((session (haskell-session-lookup name)))
        (if session
            (when (y-or-n-p (format "Session %s already exists. Use it?" name))
              session)
          (haskell-session-make name))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entry points: Tier 0

;;;###autoload
(defun haskell-session-change ()
  "Change the session for the current buffer."
  (interactive)
  (haskell-session-assign
   (or (haskell-session-new-assume-from-cabal)
       (haskell-session-choose)
       (haskell-session-new))))

;;;###autoload
(defun haskell-session ()
  "Get the Haskell session, prompt if there isn't one or fail."
  (or (haskell-session-maybe)
      (haskell-session-assign
       (or (haskell-session-from-buffer)
           (haskell-session-new-assume-from-cabal)
           (haskell-session-choose)
           (haskell-session-new)))))

;; XXX: this is not a *global* entry point, called from:
;;  - haskell-modules.el:haskell-add-import
(defun haskell-modules-session ()
  "Get the `haskell-session', throw an error if it's not available."
  (or (haskell-session-maybe)
      (haskell-session-assign
       (or (haskell-session-from-buffer)
           (haskell-session-choose)
           (error "No session associated with this buffer. Try M-x haskell-session-change or report this as a bug.")))))

;; XXX: this is not a *global* entry point, called from:
;;  - haskell-repl.el:haskell-interactive-mode-run-expr
;;  - haskell-load.el:haskell-process-look-config-changes
;;  - haskell-load.el:haskell-process-do-cabal
;;  - haskell-load.el:haskell-process-reload-devel-main
;;  - haskell.el:haskell-interactive-jump-to-error-line
;;  - haskell-interactive-mode.el:haskell-interactive-next-error-function
;;  - haskell-interactive-mode.el:haskell-interactive-process
;;  - haskell-interactive-mode.el:haskell-interactive-mode-do-presentation
;;  - haskell-interactive-mode.el:haskell-interactive-mode-set-prompt
;;  - haskell-interactive-mode.el:haskell-interactive-mode-clear
;;  - haskell-interactive-mode.el:haskell-interactive-mode-completion-at-point-function
;;  - haskell-commands.el:haskell-process-restart
;;  - haskell-commands.el:haskell-process-start
;;  - haskell-commands.el:haskell-rgrep
;;  - haskell-commands.el:haskell-mode-goto-span
;;  - haskell-commands.el:haskell-mode-find-def
;;  - haskell-commands.el:haskell-process-cd
;;  - haskell-commands.el:haskell-process-add-cabal-autogen
;;  - haskell-commands.el:haskell-process-unignore
(defun haskell-interactive-session ()
  "Get the `haskell-session', throw an error if it's not available."
  (or (haskell-session-maybe)
      (haskell-session-assign
       (or (haskell-session-from-buffer)
           (haskell-session-choose)
           (error "No session associated with this buffer. Try M-x haskell-session-change or report this as a bug.")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Session modules

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
;; Accessing the session

(defun haskell-session-current-dir (s)
  "Get the session current directory."
  (let ((dir (haskell-session-get s 'current-dir)))
    (or dir
        (error "No current directory."))))

(defun haskell-session-name (s)
  "Get the session name."
  (haskell-session-get s 'name))

(defun haskell-session-target (s)
  "Get the session build target.
If `haskell-process-load-or-reload-prompt' is nil, accept `default'."
  (let* ((maybe-target (haskell-session-get s 'target))
         (target (if maybe-target maybe-target
                   (let ((new-target
			  (if haskell-process-load-or-reload-prompt
			      (read-string "build target (empty for default):")
			    "")))
                     (haskell-session-set-target s new-target)))))
    (if (not (string= target "")) target nil)))

(defun haskell-session-set-target (s target)
  "Set the session build target."
  (haskell-session-set s 'target target))

(defun haskell-session-set-interactive-buffer (s v)
  "Set the session interactive buffer."
  (haskell-session-set s 'interactive-buffer v))

(defun haskell-session-set-process (s v)
  "Set the session process."
  (haskell-session-set s 'process v))

;;;###autoload
(defun haskell-session-process (s)
  "Get the session process."
  (haskell-session-get s 'process))

(defun haskell-session-set-cabal-dir (s v)
  "Set the session cabal-dir."
  (let ((true-path (file-truename v)))
    (haskell-session-set s 'cabal-dir true-path)
    (haskell-session-set-cabal-checksum s true-path)))

(defun haskell-session-set-current-dir (s v)
  "Set the session current directory."
  (let ((true-path (file-truename v)))
    (haskell-session-set s 'current-dir true-path)))

(defun haskell-session-set-cabal-checksum (s cabal-dir)
  "Set the session checksum of .cabal files"
  (haskell-session-set s 'cabal-checksum
                       (haskell-cabal-compute-checksum cabal-dir)))

(defun haskell-session-cabal-dir (s &optional no-prompt)
  "Get the session cabal-dir."
  (let ((dir (haskell-session-get s 'cabal-dir)))
    (if dir
        dir
      (unless no-prompt
        (let ((set-dir (haskell-cabal-get-dir)))
          (if set-dir
              (progn (haskell-session-set-cabal-dir s set-dir)
                     set-dir)
            (haskell-session-cabal-dir s)))))))

(defun haskell-session-tags-filename (s)
  "Get the filename for the TAGS file."
  (concat (haskell-session-cabal-dir s) "/TAGS"))

(defun haskell-session-modify (session key update)
  "Update the value at KEY in SESSION with UPDATE."
  (haskell-session-set
   session
   key
   (funcall update
            (haskell-session-get session key))))

(defun haskell-session-get (session key)
  "Get the SESSION's KEY value.
Returns nil if KEY not set."
  (cdr (assq key session)))

(defun haskell-session-set (session key value)
  "Set the SESSION's KEY to VALUE.
Returns newly set VALUE."
  (let ((cell (assq key session)))
    (if cell
        (setcdr cell value) ; modify cell in-place
      (setcdr session (cons (cons key value) (cdr session))) ; new cell
      value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Session teardown

;; This one is called from `haskell-session-kill' -- can you believe this?
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

;; depends-on:
;;  - haskell-interactive-mode.el:haskell-interactive-mode-echo
;;  - haskell-process.el:haskell-process-set
;;;###autoload
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
               (y-or-n-p (format "Killing `%s'. Also kill all associated buffers?" name))))
	 (existing-process (get-process (haskell-session-name session))))
    (when (processp existing-process)
      (haskell-interactive-mode-echo session "Killing process ...")
      (haskell-process-set (haskell-session-process session) 'is-restarting t)
      (delete-process existing-process))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dead code

(defun haskell-session-clear ()
  "Clear the buffer of any Haskell session choice."
  (set (make-local-variable 'haskell-session) nil))

(provide 'haskell-session)

;;; haskell-session.el ends here
