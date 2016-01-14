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

(ert-deftest haskell-cabal-guess-setting-1 ()
  (with-temp-buffer
    (let ((scriptDir
	   (file-name-directory
	     (or (symbol-file 'haskell-cabal-guess-setting-1)
		 (buffer-file-name)))))
      (set-visited-file-name (expand-file-name "test-data/Source.hs" scriptDir) t t))
    (should (equal "Simple"
		   (haskell-cabal-guess-setting "build-type")))))

(ert-deftest haskell-cabal-compute-checksum-1 ()
  (let ((scriptDir
         (file-name-directory
          (or (symbol-file 'haskell-cabal-guess-setting-1)
              (buffer-file-name)))))

    (should (equal "263e67082326a27585639420f4d42c8b"
                   (haskell-cabal-compute-checksum (expand-file-name "test-data" scriptDir))))))

(ert-deftest haskell-cabal-compute-next-prev-section-1 ()
  (let ((scriptDir
         (file-name-directory
          (or (symbol-file 'haskell-cabal-guess-setting-1)
              (buffer-file-name)))))

    (with-temp-buffer
      (insert-file-contents (expand-file-name "test-data/Test.cabal" scriptDir))
      (haskell-cabal-mode)
      (goto-char (point-min))
      (haskell-cabal-next-section)
      (haskell-cabal-next-subsection)
      (haskell-cabal-previous-subsection)
      (haskell-cabal-previous-section))))

(provide 'haskell-cabal-tests)

;;; haskell-cabal-tests.el ends here
