;;;###autoload
(defun haskell-trim (string)
  (replace-regexp-in-string
   "^[ \t\n]+" ""
   (replace-regexp-in-string
    "[ \t\n]+$" ""
    string)))

;;;###autoload
(defun haskell-string-take (string n)
  "Take n chars from string."
  (substring string
             0
             (min (length string) n)))

;;;###autoload
(defun haskell-is-prefix-of (x y)
  "Is x string a prefix of y string?"
  (string= (substring x 0 (min (length x) (length y)))
           (substring y 0 (min (length x) (length y)))))

(defun haskell-string ())

(provide 'haskell-string)
