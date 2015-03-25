(require 'ert)
(require 'haskell-indentation)
(require 'haskell-mode)
(require 'cl-lib)


(defun haskell-indentation-check-on-last-line (lines expected)
  (with-temp-buffer
    (haskell-mode)
    (haskell-indentation-mode)
    ;; insert lines
    (dolist (line lines)
      (insert line)
      (insert "\n"))
    (forward-line -1)
    (let ((indentations (haskell-indentation-find-indentations))
          (result "") )

      (dotimes (i (1+ (apply #'max indentations)))
        (setq result (concat result (if (memq i indentations)
                                        "^" " "))))

      (should (equal (car expected) result)))))


(ert-deftest haskell-indentation-check-1 ()
    "Check if '{' on its own line gets properly indented"
    (haskell-indentation-check-on-last-line
     '("function = Record"
       "     { field = 123 }")
     '("^          ^")))

(ert-deftest haskell-indentation-check-2 ()
    "Handle underscore in identifiers"
    (haskell-indentation-check-on-last-line
     '("function = do"
       "  (_x) <- return ()"
       " z")
     '("^ ^       ^")))
