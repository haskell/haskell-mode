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


(defun find-indent-and-backtab-positions (lines-above-content &optional prepare-buffer)
  (with-temp-buffer
    (if prepare-buffer
        (funcall prepare-buffer))
    (dolist (line lines-above-content)
      (insert line)
      (insert "\n"))
    ;; cursor is at the beginning of the second line now
    (let ((result-forward '(0))
	  (result-backward '()))
      (dotimes (i 9)
	(haskell-simple-indent)
	(setq result-forward (cons (current-column) result-forward)))
      (dotimes (i 9)
	(setq result-backward (cons (current-column) result-backward))
	(haskell-simple-indent-backtab))
      (setq result-backward (cons (current-column) result-backward))
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

(ert-deftest find-indent-positions-5 ()
  (should (equal '(2 4 6 13 15 17 19 24 32 40)
		 (find-indent-positions '(" f g e  e iirelevant"
                                          "a b c d"
                                          "             h idden"
                                          "                hidden"
                                          ""
			   	 	  "             e f g h"
                                          "")))))

(ert-deftest find-indent-and-backtab-positions-1 ()
  (should (equal '((0 2 4 5 8 16 24 32 40 48)
		   (0 2 4 5 8 16 24 32 40 48))
                 ;; Note: haskell-simple-indent-backtab is broken when
                 ;; it encounters TABs in source file.
		 (find-indent-and-backtab-positions '("a b c d e f g h"
						      "     long_streak")
                                                    (lambda ()
                                                      (setq indent-tabs-mode nil))))))

(ert-deftest find-indent-and-backtab-positions-1a ()
  (should (equal '((0 2 4 5 8 16 24 32 40 48)
		   (0 2 4 5 8 16 24 32 40 48))
		 (find-indent-and-backtab-positions '("a b c d e f g h"
						      "     long_streak")))))

(ert-deftest find-indent-and-backtab-positions-2 ()
  (should (equal '((0 8 10 13 20 24 27 32 35 37)
		   (0 8 10 13 20 24 27 32 35 37))
		 (find-indent-and-backtab-positions '("\tx <- return 123 {- This is a comment -}")
                                                    (lambda ()
                                                      (setq-local indent-tabs-mode nil))))))

(ert-deftest find-indent-and-backtab-positions-2a ()
  (should (equal '((0 8 10 13 20 24 27 32 35 37)
		   (0 8 10 13 20 24 27 32 35 37))
		 (find-indent-and-backtab-positions '("\tx <- return 123 {- This is a comment -}")))))

(ert-deftest find-indent-and-backtab-positions-3 ()
  (should (equal '((0 2 4 6 13 15 17 19 24 32)
                   (0 2 4 6 13 15 17 19 24 32))
		 (find-indent-and-backtab-positions '(" f g e  e iirelevant"
                                                      "a b c d"
                                                      "             h idden     x"
                                                      "                hidden      4 5"
                                                      ""
                                                      "             e f g h"
                                                      "")
                                                    (lambda ()
                                                      (setq-local indent-tabs-mode nil))))))

(ert-deftest find-indent-and-backtab-positions-3a ()
  (should (equal '((0 2 4 6 13 15 17 19 24 32)
                   (0 2 4 6 13 15 17 19 24 32))
		 (find-indent-and-backtab-positions '(" f g e  e iirelevant"
                                                      "a b c d"
                                                      "             h idden     x"
                                                      "                hidden      4 5"
                                                      ""
                                                      "             e f g h"
                                                      "")))))
