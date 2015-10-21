;;; haskell-cabal-tests.el
;;; Code:

(require 'ert)
(require 'haskell-cabal)

(ert-deftest haskell-cabal-enum-targets-1 ()
  "Test enumerating .cabal targets."
  (with-temp-buffer
    (haskell-cabal-mode)
    (let ((scriptDir
	   (file-name-directory
	     (or (symbol-file 'haskell-cabal-enum-targets-1)
		 (buffer-file-name)))))
      (setq default-directory (expand-file-name "test-data" scriptDir)))
    (should (equal '("Test" "test-1" "bench-1" "bin-1")
		   (haskell-cabal-enum-targets)))))

(provide 'haskell-cabal-tests)

;;; haskell-cabal-tests.el ends here
