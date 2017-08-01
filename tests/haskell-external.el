;; haskell-check-external.el --- Test if packages depending on haskell-mode install okay -*- lexical-binding: t; -*-

(require 'package)
(require 'bytecomp)

(defun haskell-check-external-batch-and-exit ()
  "Check if packages depending on haskell-mode bytecompile.

Haskell Mode should not break packages that depend on it and this
function checks if officially released versions still compile."

  (let ((byte-compile-error-on-warn t)
        ;; where to install temporary packages
        (package-user-dir (concat command-line-default-directory "/external/elpa"))
        ;; use haskell-mode directly from sources, it takes priority
        ;; over haskell-mode in packages. note that this works from
        ;; Emacs 24.4 onward, there is something wrong about this
        ;; variable in Emacs 24.3
        (package-directory-list (list command-line-default-directory))
        ;; packages depending on haskell-mode are in melpa
        (package-archives (cons '("melpa" . "http://melpa.milkbox.net/packages/") package-archives)))

    (unwind-protect
        (progn
          (make-directory "external")

          ;; We aren't really able to depend on errors and warnings in
          ;; bytecompilation as there are many packages that are very
          ;; inconsistent about following elisp best practices.
          (package-initialize)
          (package-refresh-contents)
          (package-install 'flycheck-haskell)
          (package-install 'flycheck-stack)
          (package-install 'ghc)
          (package-install 'intero))
      (delete-directory "external" t))))
