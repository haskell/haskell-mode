;;; haskell-indentation-tests.el --- tests for indentation module

;; Copyright (C) 2015 Haskell Mode contributors

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; These are tests for `haskell-indentation-mode'. It's easy to add new
;; tests, just...

(require 'cl-lib)
(require 'ert)
(require 'haskell-mode)
(require 'haskell-font-lock)
(require 'haskell-indentation)
(require 'haskell-indent)
(require 'haskell-simple-indent)

;;; Code:

(defsubst string-trim-left (string)
  "Remove leading whitespace from STRING."
  (if (string-match "\\`[ \t\n\r]+" string)
      (replace-match "" t t string)
    string))

(defun haskell-indentation-check (source &rest test-cases)
  "Check if `haskell-indentation-find-indentations' returns expected results.

SOURCE should be a string representing Haskell source code.  It
will be inserted in a brand-new buffer where `haskell-mode' and
`haskell-indent-mode' are enabled.  Position of point and
expected results of `haskell-indentation-find-indentations' are
described in TEST-CASES.

Every element in TEST-CASES list should have the following
structure:

    ((line column) pos0 pos1 pos2 …)

where LINE and COLUMN are coordinates of point before testing.
POS0, POS1, POS2, … are expressions representing indentation
positions.

For example:

    ((2 10) 0 7)

means that when point is placed at line 2 and column 10,
`haskell-indentation-find-indentations' should return value that's
equal to (0 7).

It's recommended to specify several test-cases per one snippet
because it helps increase coverage."
  (dolist (current test-cases)
    (cl-destructuring-bind ((line column) . result) current
      (with-temp-buffer
        (haskell-mode)
        (haskell-indentation-mode 1)
        (insert source)
        (newline)
        (font-lock-fontify-buffer)
        (goto-char (point-min))
        (forward-line (1- line))
        (move-to-column column)
        (should
         (equal (cons (list (line-number-at-pos)
                            (current-column))
                      (haskell-indentation-find-indentations))
                current))))))

(defmacro hindent-test (name source &rest test-cases)
  "Define ert test using `haskell-indentation-check'.

This little macro helps eliminate boilerplate.  It automatically
expracts prefix from NAME and uses it to name result test.  If
the prefix (everything before first space) contains asterisk *,
this test is allowed to fail.  It trims empty lines from the
beginning of SOURCE.  TEST-CASES don't need to be quoted, the
macro quotes them for you."
  (declare (indent defun))
  (let ((split-pos (cl-position ?  name)))
    (cl-destructuring-bind (test-name allow-failure doc-string)
        (if split-pos
            (let ((raw-prefix (substring name 0 split-pos)))
              (list (intern
                     (concat "haskell-indentation-check-"
                             (remove ?* raw-prefix)))
                    (cl-find ?* raw-prefix)
                    (substring name (1+ split-pos))))
          (list 'haskell-indentation-check-fixme
                nil
                name))
      `(ert-deftest ,test-name ()
         ,doc-string
         :expected-result
         ,(if allow-failure :failed :passed)
         (haskell-indentation-check
          ,(string-trim-left source)
          ,@(mapcar (lambda (x)
                      (list 'quote x))
                    test-cases))))))

(ert-deftest haskell-indentation-turns-off-haskell-indent ()
  (with-temp-buffer
    (haskell-mode)
    (haskell-indent-mode)
    (should haskell-indent-mode)
    (haskell-indentation-mode)
    (should haskell-indentation-mode)
    (should-not haskell-indent-mode)

    (haskell-indent-mode)
    (should-not haskell-indentation-mode)
    (should haskell-indent-mode)))

(ert-deftest haskell-indentation-turns-off-haskell-simple-indent ()
  (with-temp-buffer
    (haskell-mode)
    (haskell-simple-indent-mode)
    (should haskell-simple-indent-mode)
    (haskell-indentation-mode)
    (should haskell-indentation-mode)
    (should-not haskell-simple-indent-mode)

    (haskell-simple-indent-mode)
    (should-not haskell-indentation-mode)
    (should haskell-simple-indent-mode)))

(hindent-test "1 Check if '{' on its own line gets properly indented""
function = Record
       { field = 123 }"
  ((1 0) 0)
  ((2 0) 0 11))

(hindent-test "2 Handle underscore in identifiers""
function = do
  (_x) <- return ()
 z"
  ((1 0) 0)
  ((2 0) 2)
  ((3 0) 0 2 10))

(hindent-test "2u Handle underscore in identifiers""
function = do
  (_x) ← return ()
 z"
  ((1 0) 0)
  ((2 0) 2)
  ((3 0) 0 2 9))

(hindent-test "2a Handle apostrophe in identifiers""
function = do
  (_'x') <- return ()
 z"
  ((1 0) 0)
  ((2 0) 2)
  ((3 0) 0 2 12))

(hindent-test "2au Handle apostrophe in identifiers""
function = do
  (_'x') ← return ()
 z"
  ((1 0) 0)
  ((2 0) 2)
  ((3 0) 0 2 11))

(hindent-test "3 Import statememnt symbol list 1""
import Control.Concurrent
       ( forkIO,
         killThread )"
  ((1 0) 0)
  ((2 0) 0 7)
  ((3 0) 9)
  ((4 0) 0 7))

(hindent-test "4 Import statememnt symbol list 2""
import Control.Concurrent
       ( forkIO
       , killThread )"
  ((1 0) 0)
  ((2 0) 0 7)
  ((3 0) 7)
  ((4 0) 0 7))

(hindent-test "5 List comprehension""
fun = [ x | y
          , z ]"
  ((1 0) 0)
  ((2 0) 10)
  ((3 0) 0 6))

(hindent-test "5a* List comprehension""
fun = [ x | y,
            z ]"
  ((1 0) 0)
  ((2 0) 12)
  ((3 0) 0 6))

(hindent-test "6* \"let\" in list comprehension""
fun = [ f | x <- xs
          , y <- ys
          , let c = 123
          , f <- fx x y c ]"
  ((1 0) 0)
  ((2 0) 10)
  ((3 0) 10)
  ((4 0) 10)
  ((5 0) 0 6))

(hindent-test "6u* \"let\" in list comprehension""
fun = [ f | x ← xs
          , y ← ys
          , let c = 123
          , f ← fx x y c ]"
  ((1 0) 0)
  ((2 0) 10)
  ((3 0) 10)
  ((4 0) 10)
  ((5 0) 0))

(hindent-test "7a \"data\" after \"data\"""
data ABC = ABC
data DEF = DEF"
  ((1 0) 0)
  ((2 0) 0))

(hindent-test "7 \"import\" after \"import\"""
import ABC
import DEF"
  ((1 0) 0)
  ((2 0) 0)
  ((3 0) 0 7))

(hindent-test "7b* declaration after declaration" "
fun1 = undefined
fun2 = undefined"
  ((1 0) 0)
  ((2 0) 0))

(hindent-test "8* Guards in function definition""
resolve (amount, max) number
  | number > max = (1, number)
  | number == max = (amount + 1, number)"
  ((1 0) 0)
  ((2 0) 2)
  ((3 0) 0 2)
  ((4 0) 0 2))

(hindent-test "9* Operator last on line""
fun = x ++"
  ((1 0) 0)
  ((2 0) 6))

(hindent-test "10 Operator first on line""
fun = x
      ++ z"
  ((1 0) 0)
  ((2 0) 0 6))

(hindent-test "11 Guards with commas""
clunky env var1 var2
  | Just val1 <- lookup env var1
  , Just val2 <- lookup env var2"
  ((1 0) 0)
  ((2 0) 2)
  ((3 0) 2)
  ((4 0) 0 17))

(hindent-test "11u Guards with commas""
clunky env var1 var2
  | Just val1 ← lookup env var1
  , Just val2 ← lookup env var2"
  ((1 0) 0)
  ((2 0) 2)
  ((3 0) 2)
  ((4 0) 0 16))

(hindent-test "12 \"do\" as expression""
fun = do { putStrLn \"X\";
         }"
  ((1 0) 0)
  ((2 0) 9 11)
  ((3 0) 0))

(hindent-test "13* Don't indent after deriving""
data X = X
  deriving (Eq, Ord, Show)"
  ((1 0) 0)
  ((2 0) 0 2)
  ((3 0) 0))

(hindent-test "14* Line starting with operator inside \"do\" needs to be indented""
fun = do
  pure X
  something
    <*> marg"
  ((1 0) 0)
  ((2 0) 2)
  ((3 0) 0 2)
  ((4 0) 4))

(hindent-test "15* An \"if..then\" inside a \"do\" block""
fun = do
  if x
  then do
    putStrLn \"True\""
  ((1 0) 0)
  ((2 0) 2)
  ((3 0) 2)
  ((4 0) 4)
  ((5 0) 2 4))

(hindent-test "16 Lambda and a \"do\" block""
fun = \\x -> do"
  ((1 0) 0)
  ((2 0) 2))

(hindent-test "16a A lambda""
fun = \\x ->"
  ((1 0) 0)
  ((2 0) 2))

(hindent-test "16u Lambda and a do block""
fun = \\x → do"
  ((1 0) 0)
  ((2 0) 2))

(hindent-test "16au A lambda""
fun = \\x →"
  ((1 0) 0)
  ((2 0) 2))

(hindent-test "17a* A type for a function""
fun :: Int
    -> Int"
  ((1 0) 0)
  ((2 0) 4)
  ((3 0) 0 4))

(hindent-test "17au* A type for a function""
fun :: Int
    → Int"
  ((1 0) 0)
  ((2 0) 4)
  ((3 0) 0 4))

(hindent-test "17b* A type for a function with context""
fun :: Monad m
    => Int"
  ((1 0) 0)
  ((2 0) 4)
  ((3 0) 0 4))

(hindent-test "17bu* A type for a function with context""
fun ∷ Monad m
    ⇒ Int"
  ((1 0) 0)
  ((2 0) 4)
  ((3 0) 0 4))

(hindent-test "17c* A type for a function with complicated context""
fun :: (Monad m, MonadBaseControl IO m, MyMonad (A v) m)
    => MyMonad (A v) m"
  ((1 0) 0)
  ((2 0) 4)
  ((3 0) 0 4))

(hindent-test "17cu* A type for a function with complicated context""
fun ∷ (Monad m, MonadBaseControl IO m, MyMonad (A v) m)
    ⇒ MyMonad (A v) m"
  ((1 0) 0)
  ((2 0) 4)
  ((3 0) 0 4))

(hindent-test "17d* A type for a function with param and a complicated context""
fun :: (Monad m, MonadBaseControl IO m, MyMonad (A v) m)
    => MyMonad (A v) m
    -> m (Maybe a)"
  ((1 0) 0)
  ((2 0) 4)
  ((3 0) 4)
  ((4 0) 0 4))

(hindent-test "17du* A type for a function with param and a complicated context""
fun ∷ (Monad m, MonadBaseControl IO m, MyMonad (A v) m)
    ⇒ MyMonad (A v) m
    → m (Maybe a)"
  ((1 0) 0)
  ((2 0) 4)
  ((3 0) 4)
  ((4 0) 0 4))

(hindent-test "18a* \"if-then-else\" indentation: \"then\"""
x = if flag
    then 1"
  ((1 0) 0)
  ((2 0) 4)
  ((3 0) 4 9))

(hindent-test "18b \"if-then-else\" indentation: \"else\"""
x = if flag
    then 1
    else 0"
  ((1 0) 0)
  ((2 0) 4)
  ((3 0) 4)
  ((4 0) 0 9))

(hindent-test "18c* \"do\" and \"if-then-else\" indentation: \"then\"""
x = do
  if flag
  then 1"
  ((1 0) 0)
  ((2 0) 2)
  ((3 0) 2)
  ((4 0) 2))

(hindent-test "18d* \"do\" and \"if-then-else\" indentation: \"else\"""
x = do
  if flag
  then 1
  else 0"
  ((1 0) 0)
  ((2 0) 2)
  ((3 0) 2)
  ((4 0) 2))

(hindent-test "18e* \"do\" and \"if-then-else\" indentation: \"else\"""
x = do
  if flag
  then do
    return ()"
  ((1 0) 0)
  ((2 0) 2)
  ((3 0) 2)
  ((4 0) 4)
  ((5 0) 2 4))

(hindent-test "18f* \"do\" and \"if-then-else\" indentation: \"else\"""
x = do
  if flag
  then do
    return ()
  else do
    return ()"
  ((1 0) 0)
  ((2 0) 2)
  ((3 0) 2)
  ((4 0) 4)
  ((5 0) 2 4)
  ((6 0) 4)
  ((7 0) 0 2 4))

(hindent-test "19a* \"let\" and \"in\"""
x = let
  y"
  ((1 0) 0)
  ((2 0) 2)
  ((3 0) 2))

(hindent-test "19b \"let\" and \"in\"" "
x = let y
    in
      z"
  ((1 0) 0)
  ((2 0) 4)
  ((3 0) 6))

(hindent-test "19c* \"let\" in a \"do\"""
x = do
  thing
  let
    z = 5"
  ((1 0) 0)
  ((2 0) 2)
  ((3 0) 2)
  ((4 0) 4))

(hindent-test "20a* \"instance\" declaration""
instance C a where
  c = undefined"
  ((1 0) 0)
  ((2 0) 2)
  ((3 0) 0 2))

(hindent-test "20b* \"instance\" declaration""
instance (Monad m) => C m a where
  c = undefined"
  ((1 0) 0)
  ((2 0) 2)
  ((3 0) 0 2))

(hindent-test "20bu* \"instance\" declaration""
instance (Monad m) ⇒ C m a where
  c = undefined"
  ((1 0) 0)
  ((2 0) 2)
  ((3 0) 0 2))

(hindent-test "21a* fix \"let\" statement in \"do\" block""
main :: IO ()
main = do
let foo = Foo {
      bar = 0
    , baz = 0"
  ((1 0) 0)
  ((2 0) 0)
  ((3 0) 2)
  ((4 0) 6)
  ((5 0) 4)
  ((6 0) 4))

(hindent-test "21b* fix named fields in \"data\" declaration""
data Foo = Foo {
  bar :: Int
, baz :: Int"
  ((1 0) 0)
  ((2 0) 4)
  ((3 0) 2)
  ((4 0) 2))

(hindent-test "21c* fix \"let\" statement and record in \"do\" block""
main :: IO ()
main = do
let foo = Foo {
            bar = 0
            , baz = 0"
  ((1 0) 0)
  ((2 0) 0)
  ((3 0) 2)
  ((4 0) 14)
  ((5 0) 12)
  ((6 0) 12))

(hindent-test "22 should obey layout only outside parentheses" "
func = 1234
  where
    foo :: Ivory eff ()
    foo = do
      return ()"
  ((1 0) 0)
  ((2 0) 2)
  ((3 0) 4)
  ((4 0) 0 4 11)
  ((5 0) 6))

(hindent-test "23* should not fail when seeing comments" "
-- important non-empty line
{-
-}"
  ((3 2) 0))

(hindent-test "24 should parse inline type signatures properly" "
foo = do
  _ :: String <- undefined
  _ :: String <- undefined
  return ()"
              ((1 0) 0)
              ((2 0) 2)
              ((3 0) 0 2 17)
              ((4 0) 0 2 17))

(hindent-test "25a* support scoped type declarations" "
foo = do
  bar :: String
      -> String
    <- undefined"
              ((1 0) 0)
              ((2 0) 2)
              ((3 0) 6 9)
              ;; here it brakes, it would like to put '<-' on same line with 'bar'
              ;; the culprit is the 'do' keyword
              ((4 0) 4))

(hindent-test "25b support scoped type declarations" "
foo = let
  bar :: String
      -> String
    = undefined"
              ((1 0) 0)
              ((2 0) 2)
              ((3 0) 6 9)
              ((4 0) 4))

(hindent-test "26 should parse unindented where-clause properly" "
foo = do
    return ()
  where
    bar = undefined"
              ((4 0) 4))
              
(hindent-test "27* expecting then (GH-884)" "
foo = do
    if True
    then return ()
"
              ((4 0) 4))

(hindent-test "28a names starting with quotes" "
f = a (a 'A)
    (a 'A)
"
              ((2 0) 0 4))

(hindent-test "28b character literal (escape sequence)" "
f = '\\\\'

"
              ((2 0) 0 4))


(hindent-test "28c name starting with a quote" "
function (Operation 'Init) = do
  print 'Init
"
              ((2 0) 2))

(hindent-test "29a quasiquote single line" "
test = [randomQQ| This is a quasiquote with the word in |]

"
              ((2 0) 0 7))

(hindent-test "29b quasiquote multiple lines" "
test = [randomQQ| This is
          a quasiquote
          with the word in |]

"
              ((4 0) 0 7))
(hindent-test "30* parse '[] identifier correctly" "
instance Callable '[]
"
	      ((1 0) 2))
(hindent-test "31* allow type class declaration without methods" "
class Foo a where
instance Bar Int
"
	      ((2 0) 0))
(hindent-test "32 allow type operators" "
data (:.) a b = a :. b
"
	      ((2 0) 0 16))
(hindent-test "33* parse #else in CPP" "
#ifdef FLAG
foo = ()
#else
"
	      ((4 0) 0))


(hindent-test "34 beginning of line inside parentheses" "
data T = T {
  foo :: String
, bar :: String
}

"
              ;; set of answers isn't best but it is not a bug
              ;; should be just 0
              ((5 0) 0 9))

(hindent-test "35 baroque construct which causes parse error" "
az = Projection
  { unproject = do
        case x of
          _ -> return
  , maxR = pi
  }
"
	      ((6 0) 2))

(hindent-test "35a parse a backslash properly" "
az = Projection
  { unproject = \\x -> do
        case x of
          _ -> return
  , maxR = pi
  }
"
	      ((6 0) 2))

(hindent-test "36 yet another parser failure" "
tokOpenTag =
  asum [ do void
       , return
       ]
"
	      ((4 0) 7))
(hindent-test "37* Indent continuation lines in multiline string literal" "
a = \"multiline\\
"
	      ((2 0) 4))

(hindent-test "38 Indent in do block after multiline string literal" "
s = do
  a <- \"multiline\\
       \\ line 2\"
"
       ((4 0) 0 2 7))

(hindent-test "39 do not crash after two multiline literals in do block" "
servePost = do
  a <- fun \"line 1\\
           \\line 2\"
  b <- queryT \"comma is important: , \\
             \\ line 2 \"
"
	      ((6 0) 0 2 7))

(hindent-test "40 parse error in multiline tuple" "
a = ( 1
, "
	      ((2 0) 4)
	      ((2 2) 6))

(hindent-test "41 open do inside a list" "
x = asum [ withX $ do
             return ()
         ]
"
	      ((2 0) 13))

(hindent-test "42 open do inside a list second element" "
x = asum [ mzero
         , withX $ do
             return ()
         ]
"
	      ((3 0) 13))

(hindent-test "43 open do inside a list second element, reset alignment" "
x = asum [ mzero
             , withX $ do
                 return ()
         ]
"
	      ((3 0) 17))

(hindent-test "44 expression continues, reset alignment" "
function = abc
       def
       xyz"
  ((3 0) 0 7))


(ert-deftest haskell-indentation-ret-indents ()
  (switch-to-buffer (get-buffer-create "source.hs"))
  (haskell-mode)
  (insert "main = do")
  (execute-kbd-macro (kbd "<RET>"))
  (should (equal 2 (count-lines (point-min) (point))))
  (should (equal 2 (- (point) (line-beginning-position)))))

(ert-deftest haskell-indentation-tab-and-backtab ()
  (switch-to-buffer (get-buffer-create "source.hs"))
  (haskell-mode)
  (insert "main = do\n  lala\n  ")
  (execute-kbd-macro (kbd "TAB"))
  (should (equal 4 (count-lines (point-min) (point))))
  (should (equal 4 (- (point) (line-beginning-position))))
  (execute-kbd-macro (kbd "<backtab>"))
  (should (equal 4 (count-lines (point-min) (point))))
  (should (equal 2 (- (point) (line-beginning-position)))))


;;; haskell-indentation-tests.el ends here
