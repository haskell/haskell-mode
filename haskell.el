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
(require 'haskell-interactive-mode)
(require 'haskell-presentation-mode)

(defun haskell-process-do-simple-echo (line &optional mode)
  "Send LINE to the GHCi process and echo the result in some
fashion, such as printing in the minibuffer, or using
haskell-present, depending on configuration."
  (let ((process (haskell-process)))
    (haskell-process-queue-command
     process
     (make-haskell-command
      :state (list process line mode)
      :go (lambda (state)
            (haskell-process-send-string (car state) (cadr state)))
      :complete (lambda (state response)
                  ;; TODO: TBD: don't do this if
                  ;; `haskell-process-use-presentation-mode' is t.
                  (haskell-interactive-mode-echo
                   (haskell-process-session (car state))
                   response
                   (cl-caddr state))
                  (if haskell-process-use-presentation-mode
                      (progn (haskell-present (cadr state)
                                              (haskell-process-session (car state))
                                              response)
                             (haskell-session-assign
                              (haskell-process-session (car state))))
                    (haskell-mode-message-line response)))))))

;;;###autoload
(defun haskell-process-do-type (&optional insert-value)
  "Print the type of the given expression."
  (interactive "P")
  (if insert-value
      (haskell-process-insert-type)
    (haskell-process-do-simple-echo
     (let ((ident (haskell-ident-at-point)))
       ;; TODO: Generalize all these `string-match' of ident calls into
       ;; one function.
       (format (if (string-match "^[_[:lower:][:upper:]]" ident)
                   ":type %s"
                 ":type (%s)")
               ident))
     'haskell-mode)))

;;;###autoload
(defun haskell-process-do-info (&optional prompt-value)
  "Print info on the identifier at point.
If PROMPT-VALUE is non-nil, request identifier via mini-buffer."
  (interactive "P")
  (haskell-process-do-simple-echo
   (let ((ident (if prompt-value
                    (read-from-minibuffer "Info: " (haskell-ident-at-point))
                  (haskell-ident-at-point)))
         (modname (unless prompt-value
                    (haskell-utils-parse-import-statement-at-point))))
     (if modname
         (format ":browse! %s" modname)
       (format (if (string-match "^[a-zA-Z_]" ident)
                   ":info %s"
                 ":info (%s)")
               (or ident
                   (haskell-ident-at-point)))))
   'haskell-mode))

(provide 'haskell)
