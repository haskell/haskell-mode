;;; haskell-compile.el --- Haskell/GHC compilation sub-mode -*- lexical-binding: t -*-

;; Copyright (C) 2013  Herbert Valerio Riedel
;; Copyright (C) 2017  Vasantha Ganesh Kanniappan <vasanthaganesh.k@tuta.io>

;; Author: Herbert Valerio Riedel <hvr@gnu.org>

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Simple GHC-centric compilation sub-mode; see info node
;; `(haskell-mode)compilation' for more information

;;; Code:

(require 'compile)
(require 'haskell-customize)

;;;###autoload
(defgroup haskell-compile nil
  "Settings for Haskell compilation mode"
  :link '(custom-manual "(haskell-mode)compilation")
  :group 'haskell)

(defcustom haskell-compile-ghc-filter-linker-messages
  t
  "Filter out unremarkable \"Loading package...\" linker messages during compilation."
  :group 'haskell-compile
  :type 'boolean)

(defconst haskell-compilation-error-regexp-alist
  `((,(concat
       "^ *\\(?1:[^\t\r\n]+?\\):"
       "\\(?:"
       "\\(?2:[0-9]+\\):\\(?4:[0-9]+\\)\\(?:-\\(?5:[0-9]+\\)\\)?" ;; "121:1" & "12:3-5"
       "\\|"
       "(\\(?2:[0-9]+\\),\\(?4:[0-9]+\\))-(\\(?3:[0-9]+\\),\\(?5:[0-9]+\\))" ;; "(289,5)-(291,36)"
       "\\)"
       ":\\(?6:\n?[ \t]+[Ww]arning:\\)?")
     1 (2 . 3) (4 . 5) (6 . nil)) ;; error/warning locus

    ;; multiple declarations
    ("^    \\(?:Declared at:\\|            \\) \\(?1:[^ \t\r\n]+\\):\\(?2:[0-9]+\\):\\(?4:[0-9]+\\)$"
     1 2 4 0) ;; info locus

    ;; this is the weakest pattern as it's subject to line wrapping et al.
    (" at \\(?1:[^ \t\r\n]+\\):\\(?2:[0-9]+\\):\\(?4:[0-9]+\\)\\(?:-\\(?5:[0-9]+\\)\\)?[)]?$"
     1 2 (4 . 5) 0)) ;; info locus
  "Regexps used for matching GHC compile messages.
See `compilation-error-regexp-alist' for semantics.")

(defvar haskell-compilation-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map compilation-mode-map))
  "Keymap for `haskell-compilation-mode' buffers.
This is a child of `compilation-mode-map'.")

(defun haskell-compilation-filter-hook ()
  "Local `compilation-filter-hook' for `haskell-compilation-mode'."

  (when haskell-compile-ghc-filter-linker-messages
    (delete-matching-lines "^ *Loading package [^ \t\r\n]+ [.]+ linking [.]+ done\\.$"
                           (save-excursion (goto-char compilation-filter-start)
                                           (line-beginning-position))
                           (point))))

(define-compilation-mode haskell-compilation-mode "HsCompilation"
  "Haskell/GHC specific `compilation-mode' derivative.
This mode provides support for GHC 7.[46]'s compile
messages. Specifically, also the `-ferror-spans` source location
format is supported, as well as info-locations within compile
messages pointing to additional source locations."
  (setq-local compilation-error-regexp-alist
              haskell-compilation-error-regexp-alist)

  (add-hook 'compilation-filter-hook
            'haskell-compilation-filter-hook nil t))

;;;###autoload
(defun haskell-compile ()
  "Compile the Haskell program including the current buffer.
Tries to locate the next cabal description in current or parent
folders, `stack.yaml' file via `locate-dominating-file'. If they
are not found, then the haskell file in the currebt buffer is
executed.

`haskell-compile' uses `haskell-compilation-mode' which is
derived from `compilation-mode'. See Info
node `(haskell-mode)compilation' for more details."
  (interactive)
  (save-some-buffers (not compilation-ask-about-save)
                     compilation-save-buffers-predicate)
  (let* ((commandl (cl-ecase (haskell-process-type)
                     ('ghci `(,haskell-process-path-ghc
                              ,(buffer-file-name)
                              ,haskell-process-args-ghc))
                     ('cabal-repl `(,haskell-process-path-cabal
                                    "build"
                                    ,haskell-process-args-cabal-build))
                     ('stack-ghci `(,haskell-process-path-stack
                                    "build"
                                    ,haskell-process-args-stack-build))))
         (command (mapconcat #'concat commandl " ")))
    (message command)
    (compilation-start command 'haskell-compilation-mode)))

(provide 'haskell-compile)
;;; haskell-compile.el ends here
