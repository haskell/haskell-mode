;;; w3m-haddock-tests.el --- Tests for w3m-haddocks    -*- lexical-binding: t -*-
;;; Summary:
;;; Code:

(require 'ert)
(require 'f)
(require 'haskell)
(require 'w3m-haddock) ;; implementation under test

(defmacro with-temp-directory (tdir &rest body)
  "Cleate TDIR temp directory and execute BODY."
  `(let ((,tdir (make-temp-file "tmpdir" t)))
    (unwind-protect
        (progn ,@body))
    (delete-directory ,tdir t)))

(ert-deftest w3m-haddock-discover-ref-test ()
    (with-temp-directory
     d
     (let ((haddock-root (concat d "/nix/store/53195319531953-ghc-with-packages/share/doc")))
       (make-directory haddock-root t)
       (f-write-text haddock-root nil (concat d "/.haddock-ref"))
       (make-directory (concat d "/src/a/b/c") t)
       (f-write-text "module Main where" nil (concat d "/src/a/b/c/Main.hs"))
       (find-file (concat d "/src/a/b/c/Main.hs"))
       (should (equal haddock-root (w3m-haddock-discover-ref))))))

(ert-deftest w3m-haddock-discover-ref-nil-test ()
  (with-temp-directory
   d
   (make-directory (concat d "/src/a/b/c") t)
   (f-write-text "module Main where" nil (concat d "/src/a/b/c/Main.hs"))
   (find-file (concat d "/src/a/b/c/Main.hs"))
   (should (null (w3m-haddock-discover-ref)))))

(provide 'w3m-haddock-tests)
;;; w3m-haddock-tests.el ends here
