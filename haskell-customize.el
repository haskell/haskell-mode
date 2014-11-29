;;; haskell-customize.el --- Customization settings

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

(require 'cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization variables

(defcustom haskell-completing-read-function 'ido-completing-read
  "Default function to use for completion."
  :group 'haskell
  :type '(choice
          (function-item :tag "ido" :value ido-completing-read)
          (function-item :tag "helm" :value helm--completing-read-default)
          (function-item :tag "completing-read" :value completing-read)
          (function :tag "Custom function")))

(defcustom haskell-process-type
  'auto
  "The inferior Haskell process type to use."
  :type '(choice (const auto) (const ghci) (const cabal-repl) (const cabal-dev) (const cabal-ghci))
  :group 'haskell-interactive)

(defcustom haskell-ask-also-kill-buffers
  t
  "Ask whether to kill all associated buffers when a session
 process is killed."
  :type 'boolean
  :group 'haskell-interactive)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Accessor functions

(defun haskell-process-type ()
  "Return `haskell-process-type', or a guess if that variable is 'auto."
  (if (eq 'auto haskell-process-type)
      (if (locate-dominating-file
           default-directory
           (lambda (d)
             (or (file-directory-p (expand-file-name ".cabal-sandbox" d))
                 (cl-find-if (lambda (f) (string-match-p ".\\.cabal\\'" f)) (directory-files d)))))
          'cabal-repl
        'ghci)
    haskell-process-type))

(provide 'haskell-customize)
