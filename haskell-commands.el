;;; haskell-commands.el --- Commands that can be run on the process

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

(require 'haskell-process)

(defun haskell-commands-process ()
  "Get the Haskell session, throws an error if not available."
  (or (haskell-session-process (haskell-session-maybe))
      (error "No Haskell session/process associated with this
      buffer. Maybe run M-x haskell-session-change?")))

(defun haskell-process-clear ()
  "Clear the current process."
  (interactive)
  (haskell-process-reset (haskell-commands-process))
  (haskell-process-set (haskell-commands-process) 'command-queue nil))

(defun haskell-process-interrupt ()
  "Interrupt the process (SIGINT)."
  (interactive)
  (interrupt-process (haskell-process-process (haskell-commands-process))))

(defun haskell-process-reload-with-fbytecode (process module-buffer)
  "Reload FILE-NAME with -fbyte-code set, and then restore -fobject-code."
  (haskell-process-queue-without-filters process ":set -fbyte-code")
  (haskell-process-touch-buffer process module-buffer)
  (haskell-process-queue-without-filters process ":reload")
  (haskell-process-queue-without-filters process ":set -fobject-code"))

(defun haskell-process-touch-buffer (process buffer)
  "Updates mtime on the file for BUFFER by queing a touch on
PROCESS."
  (interactive)
  (haskell-process-queue-command
   process
   (make-haskell-command
    :state (cons process buffer)
    :go (lambda (state)
          (haskell-process-send-string
           (car state)
           (format ":!%s %s"
                   "touch"
                   (shell-quote-argument (buffer-file-name
                                          (cdr state))))))
    :complete (lambda (state _)
                (with-current-buffer (cdr state)
                  (clear-visited-file-modtime))))))

(provide 'haskell-commands)
