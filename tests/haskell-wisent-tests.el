;;; haskell-wisent-tests.el --- Haskell grammar tests -*- lexical-binding: t -*-

;; Copyright (c) 2016 Gracjan Polak. All rights reserved.

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

;;; Code:

(require 'ert)
(require 'haskell-wy)

(defmacro ws (start source expected)
  `(with-temp-buffer
     (haskell-mode)
     (semantic-mode)
     (insert ,source)
     (let* ((wisent-parse-verbose-flag t)
            (wisent-collected-messages nil)
            (wisent-collect-message (lambda (msg)
                                      (push msg wisent-collected-messages)))
            (wisent-lex-istream
             (funcall semantic-lex-analyzer (point-min) (point-max)))
            (wisent-result (wisent-parse semantic--parse-table #'wisent-lex wisent-collect-message ',start))
            (wisent-result-cut
             (if (consp wisent-result)
                 (list (nth 0 wisent-result) (nth 1 wisent-result)
                       (nth 2 wisent-result) (nth 3 wisent-result) (nth 4 wisent-result))
               wisent-result)))
       (should (equal nil wisent-collected-messages))
       (should (equal ',expected wisent-result-cut)))))

(ert-deftest haskell-wisent-funlhs-1 ()
  (ws decl "
fun"
      ("fun" function nil nil nil)))

(ert-deftest haskell-wisent-funlhs-2 ()
  (ws decl "
fun x y"
      ("fun" function nil nil nil)))

(ert-deftest haskell-wisent-funlhs-3 ()
  (ws decl "
fun x Nothing"
      ("fun" function nil nil nil)))

(ert-deftest haskell-wisent-funlhs-3a ()
  (ws decl "
fun x (Just y)"
      ("fun" function nil nil nil)))

(ert-deftest haskell-wisent-funlhs-4 ()
  (ws decl "
fun x (Rec {})"
      ("fun" function nil nil nil)))

(ert-deftest haskell-wisent-funlhs-4a ()
  (ws decl "
fun x (Rec {x = Maybe g})"
      ("fun" function nil nil nil)))

(ert-deftest haskell-wisent-funlhs-5 ()
  (ws decl "
fun (Nothing) (x:xs)"
      ("fun" function nil nil nil)))

(ert-deftest haskell-wisent-funlhs-6 ()
  (ws decl "
fun a@b"
      ("fun" function nil nil nil)))
