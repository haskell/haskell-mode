(require 'ert)
(require 'haskell-indentation)
(require 'haskell-mode)
(require 'cl-lib)


(defun haskell-indentation-check (&rest lines)
  "Check if `haskell-indentation-find-indentations` returns expected list of positions.

LINES is the lines to insert to a temp buffer. Last line is not
inserted, it is treated as a pattern of indentation points marked
by '^' characters in positions. Point will be set on the *last*
line inserted in the buffer so if you need to test indentation
*past* source code on empty line then an empty line must be
specified.

Example of lines:

\"func = do\"
\"      x\"
\"  ^\"
"
  (with-temp-buffer
    (haskell-mode)
    (haskell-indentation-mode)

    (let (expected
          indentations
          (result ""))
      (dolist (line lines)
        (when expected
          (insert expected)
          (insert "\n"))
        (setq expected line))

      (forward-line -1)
      (setq indentations (haskell-indentation-find-indentations))

      (dotimes (i (1+ (apply #'max indentations)))
        (setq result (concat result (if (memq i indentations)
                                        "^" " "))))

      (should (equal expected result)))))


(ert-deftest haskell-indentation-check-1 ()
  "Check if '{' on its own line gets properly indented"
  (haskell-indentation-check
   "function = Record"
   "       { field = 123 }"
   "^          ^"))

(ert-deftest haskell-indentation-check-2 ()
  "Handle underscore in identifiers"
  (haskell-indentation-check
   "function = do"
   "  (_x) <- return ()"
   " z"
   "^ ^       ^"))

(ert-deftest haskell-indentation-check-2a ()
  "Handle apostrophe in identifiers"
  (haskell-indentation-check
   "function = do"
   "  (_'x') <- return ()"
   " z"
   "^ ^         ^"))


(ert-deftest haskell-indentation-check-3 ()
  "Import statememnt symbol list 1"
  (haskell-indentation-check
   "import Control.Concurrent"
   "  ( forkIO,"
   "    killThread)"
   "    ^"))

(ert-deftest haskell-indentation-check-4 ()
  "Import statememnt symbol list 2"
  :expected-result :failed
  (haskell-indentation-check
   "import Control.Concurrent"
   "  ( forkIO"
   "  , killThread)"
   "  ^"))

(ert-deftest haskell-indentation-check-5 ()
  "List comprehension"
  (haskell-indentation-check
   "fun = [ x | y"
   "          , z ]"
   "          ^"))

(ert-deftest haskell-indentation-check-5a ()
  "List comprehension"
  :expected-result :failed
  (haskell-indentation-check
   "fun = [ x | y,"
   "            z ]"
   "            ^"))

(ert-deftest haskell-indentation-check-6 ()
  "let in list comprehension"
  :expected-result :failed
  (haskell-indentation-check
   "fun = [ f | x <- xs"
   "          , y <- ys"
   "          , let c = 123"
   "          , f <- fx x y c ]"
   "          ^"))

(ert-deftest haskell-indentation-check-7 ()
  "import after import"
  :expected-result :failed
  (haskell-indentation-check
   "import ABC"
   "import DEF"
   "^"))

(ert-deftest haskell-indentation-check-8 ()
  "Guards in function definition"
  (haskell-indentation-check
   "resolve (amount, max) number"
   "  | number > max = (1, number)"
   "  | number == max = (amount + 1, number)"
   "  ^"))

(ert-deftest haskell-indentation-check-9 ()
  "Operator last on line"
  :expected-result :failed
  (haskell-indentation-check
   "fun = x ++"
   "      ^"))

(ert-deftest haskell-indentation-check-10 ()
  "Operator first on line"
  :expected-result :failed
  (haskell-indentation-check
   "fun = x"
   "      ++ z"
   "      ^"))

(ert-deftest haskell-indentation-check-11 ()
  "Guards with commas"
  (haskell-indentation-check
   "clunky env var1 var2"
   "  | Just val1 <- lookup env var1"
   "  , Just val2 <- lookup env var2"
   "  ^"))

(ert-deftest haskell-indentation-check-12 ()
  "Guards with commas"
  :expected-result :failed
  (haskell-indentation-check
   "fun = do { putStrLn \"X\";"
   "         }"
   "         ^"))

(ert-deftest haskell-indentation-check-13 ()
  "Don't indent after deriving"
  :expected-result :failed
  (haskell-indentation-check
   "data X = X"
   "  deriving (Eq, Ord, Show)"
   "^"))

(ert-deftest haskell-indentation-check-14 ()
  "Line starting with operator inside a 'do' needs to be indented"
  :expected-result :failed
  (haskell-indentation-check
   "fun = do"
   "  pure X"
   "    <*> marg"
   "    ^"))

(ert-deftest haskell-indentation-check-15 ()
  "An if..then inside a do block"
  :expected-result :failed
  (haskell-indentation-check
   "fun = do"
   "  if x"
   "    then do"
   "      putStrLn \"True\""
   "      ^"))

(ert-deftest haskell-indentation-check-16 ()
  "Lambda and a do block"
  :expected-result :failed
  (haskell-indentation-check
   "fun = \x -> do"
   "  ^"))

(ert-deftest haskell-indentation-check-16a ()
  "A lambda"
  :expected-result :failed
  (haskell-indentation-check
   "fun = \x ->"
   "  ^"))
