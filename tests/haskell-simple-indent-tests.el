(require 'ert)
(require 'haskell-simple-indent)

(defun find-indent-positions (lines-above-content)
  (with-temp-buffer
    (dolist (line lines-above-content)
      (insert line)
      (insert "\n"))
    ;; cursor is at the beginning of the second line now
    (let ((result '()))
      (dotimes (i 10)
	(haskell-simple-indent)
	(setq result (cons (current-column) result)))
      (reverse result))))


(defun find-indent-and-backtab-positions (lines-above-content)
  (with-temp-buffer
    (dolist (line lines-above-content)
      (insert line)
      (insert "\n"))
    ;; cursor is at the beginning of the second line now
    (let ((result-forward '())
	  (result-backward '()))
      (dotimes (i 10)
	(haskell-simple-indent)
	(setq result-forward (cons (current-column) result-forward)))
      (dotimes (i 10)
	(setq result-backward (cons (current-column) result-backward))
	(haskell-simple-indent-backtab))
      (list (reverse result-forward) result-backward))))

(ert-deftest find-indent-positions-1 ()
  (should (equal '(5 7 10 19 26 32 40 48 56 64)
		 (find-indent-positions '("main = do putStrLn \"Hello World!\"")))))


(ert-deftest find-indent-positions-2 ()
  (should (equal '(8 10 13 20 24 27 32 35 37 45)
		 (find-indent-positions '("\tx <- return 123 {- This is a comment -}")))))


(ert-deftest find-indent-positions-3 ()
  (should (equal '(2 4 6 8 10 12 14 16 24 32)
		 (find-indent-positions '("a b c d"
					  "        e f g h")))))


(ert-deftest find-indent-positions-4 ()
  (should (equal '(2 4 5 8 16 24 32 40 48 56)
		 (find-indent-positions '("a b c d e f g h"
					  "     long_streak")))))

(ert-deftest find-indent-and-backtab-positions-1 ()
  :expected-result :failed
  (should (equal '((2 4 5 8 16 24 32 40 48 56)
		   (2 4 5 8 16 24 32 40 48 56))
		 (find-indent-and-backtab-positions '("a b c d e f g h"
						      "     long_streak")))))

(ert-deftest find-indent-and-backtab-positions-2 ()
  :expected-result :failed
  (should (equal '((8 10 13 20 24 27 32 35 37 45)
		   (8 10 13 20 24 27 32 35 37 45))
		 (find-indent-and-backtab-positions '("\tx <- return 123 {- This is a comment -}")))))
