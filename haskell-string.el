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
  (string= x (substring y 0 (min (length y) (length x)))))

;;;###autoload
(defun haskell-string-ellipsis (string n)
  "Ellipsize a string."
  (let ((e (haskell-string-take string n)))
    (if (> (length string) (length e))
        (concat e "…")
      string)))

(defun haskell-string ())

(provide 'haskell-string)
