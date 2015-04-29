;;; haskell-presentation-mode.el --- Presenting Haskell things

;; Copyright (C) 2013  Chris Done

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

;;; Code:

(require 'haskell-mode)
(require 'haskell-session)

(define-derived-mode haskell-presentation-mode
  haskell-mode "Presentation"
  "Major mode for viewing Haskell snippets.
          \\{hypertext-mode-map}"
  (setq case-fold-search nil))

(define-key haskell-presentation-mode-map (kbd "q") 'quit-window)

(defun haskell-present (name session code)
  "Present given code in a popup buffer.
Creates temporal buffer with given NAME and assigns it to given
haskell SESSION; presented CODE will be fontified as haskell code."
  (let ((buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (haskell-presentation-mode)

      (when (boundp 'shm-display-quarantine)
        (set (make-local-variable 'shm-display-quarantine) nil))

      (let ((buffer-read-only nil)
            (hint "-- Hit `q' to close this window.\n\n"))
        (haskell-session-assign session)
        (erase-buffer)
        (insert hint)
        (save-excursion
          (insert code "\n\n")))
      (setq buffer-read-only t))

    (if (eq major-mode 'haskell-presentation-mode)
        (switch-to-buffer buffer)
      (pop-to-buffer buffer))))

(provide 'haskell-presentation-mode)

;;; haskell-presentation-mode.el ends here
