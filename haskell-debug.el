;;; haskell-debug.el --- Debugging mode via GHCi

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

(defun haskell-debug ()
  "Start the debugger for the current Haskell (GHCi) session."
  (interactive)
  (let ((session (haskell-session)))
    (switch-to-buffer-other-window (haskell-debug-buffer-name))
    (haskell-debug-mode)
    (setq haskell-session session)))

(defun haskell-debug/step (&optional expr)
  "Step into the next function."
  (interactive)
  (let* ((string
          (haskell-process-queue-sync-request
           (haskell-process)
           (if expr
               (concat ":step " expr)
             ":step"))))
    (cond
     ((string= string "not stopped at a breakpoint")
      (call-interactively 'haskell-debug/start-step))
     (t (let ((maybe-stopped-at (haskell-debug-parse-stopped-at string)))
          (cond
           (maybe-stopped-at
            (message "Computation paused."))
           (t
            (message "Computation finished."))))))))

(defun haskell-debug/start-step (expr)
  "Start stepping EXPR."
  (interactive (list (read-from-minibuffer "Expression to step through: ")))
  (haskell-debug/step expr))

(defun haskell-debug/refresh ()
  "Refresh the debugger buffer."
  (interactive)
  (with-current-buffer (haskell-debug-buffer-name)
    (let ((inhibit-read-only t))
      (when (= (point-min)
               (point-max))
        (insert "Debugging "
                (haskell-session-name (haskell-session))
                "\n")))))

(defun haskell-debug/breakpoint-numbers ()
  "List breakpoint numbers."
  (interactive)
  (let ((breakpoints (mapcar (lambda (breakpoint)
                               (number-to-string (plist-get breakpoint :number)))
                             (haskell-debug-get-breakpoints))))
    (if (null breakpoints)
        (message "No breakpoints.")
      (message "Breakpoint(s): %s"
               (mapconcat #'identity
                          breakpoints
                          ", ")))))

(defun haskell-debug/abandon ()
  "Abandon the current computation."
  (interactive)
  (haskell-process-queue-sync-request (haskell-process) ":abandon")
  (message "Computation abandoned."))

(defun haskell-debug/break-on-function (ident)
  "Break on function IDENT."
  (interactive (list (read-from-minibuffer "Function: "
                                           (haskell-ident-at-point))))
  (haskell-process-queue-sync-request
   (haskell-process)
   (concat ":break "
           ident))
  (message "Breaking on function: %s" ident))

(defun haskell-debug-buffer-name ()
  "The debug buffer name for the current session."
  (format "*debug:%s*"
          (haskell-session-name (haskell-session))))

(define-derived-mode haskell-debug-mode
  text-mode "Debug"
  "Major mode for debugging Haskell via GHCi."
  (setq buffer-read-only t)
  (haskell-debug/refresh))

(define-key haskell-debug-mode-map (kbd "g") 'haskell-debug/refresh)
(define-key haskell-debug-mode-map (kbd "s") 'haskell-debug/step)
(define-key haskell-debug-mode-map (kbd "b") 'haskell-debug/break-on-function)

(defun haskell-debug-get-breakpoints ()
  "Get the list of breakpoints currently set."
  (let ((string (haskell-process-queue-sync-request
                 (haskell-process)
                 ":show breaks")))
    (if (string= string "No active breakpoints.")
        (list)
      (mapcar #'haskell-debug-parse-break-point
              (split-string
               string
               "\n")))))

(defun haskell-debug-parse-stopped-at (string)
  "Parse the location stopped at from the given string.

For example:

Stopped at /home/foo/project/src/x.hs:6:25-36

"
  (let ((index (string-match "Stopped at \\([^:]+\\):\\(.+\\)\n"
                             string)))
    (when index
      (list :path (match-string 1 string)
            :span (haskell-debug-parse-span (match-string 2 string))
            :types (cdr (split-string (substring string index)
                                      "\n"))))))

(defun haskell-debug-parse-break-point (string)
  "Parse a breakpoint number, module and location from a string.

For example:

[13] Main /home/foo/src/x.hs:(5,1)-(6,37)

"
  (if (string-match "^\\[\\([0-9]+\\)\\] \\([^ ]+\\) \\([^:]+\\):\\(.+\\)$"
                    string)
      (list :number (string-to-number (match-string 1 string))
            :module (match-string 2 string)
            :path (match-string 3 string)
            :span (haskell-debug-parse-span (match-string 4 string)))
    (error "Unable to parse breakpoint from string: %s"
           string)))

(defun haskell-debug-parse-span (string)
  "Parse a source span from a string.

Examples:

  (5,1)-(6,37)
  6:25-36
  5:20

People like to make other people's lives interesting by making
variances in source span notation."
  (cond
   ((string-match "\\([0-9]+\\):\\([0-9]+\\)-\\([0-9]+\\)"
                  string)
    (list :start-line (string-to-number (match-string 1 string))
          :start-col (string-to-number (match-string 2 string))
          :end-line (string-to-number (match-string 1 string))
          :end-col (string-to-number (match-string 3 string))))
   ((string-match "\\([0-9]+\\):\\([0-9]+\\)"
                  string)
    (list :start-line (string-to-number (match-string 1 string))
          :start-col (string-to-number (match-string 2 string))
          :end-line (string-to-number (match-string 1 string))
          :end-col (string-to-number (match-string 2 string))))
   ((string-match "(\\([0-9]+\\),\\([0-9]+\\))-(\\([0-9]+\\),\\([0-9]+\\))"
                  string)
    (list :start-line (string-to-number (match-string 1 string))
          :start-col (string-to-number (match-string 2 string))
          :end-line (string-to-number (match-string 3 string))
          :end-col (string-to-number (match-string 4 string))))
   (t (error "Unable to parse source span from string: %s"
             string))))

(provide 'haskell-debug)
