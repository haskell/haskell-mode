;;; inferior-haskell-tests.el --- tests for collapse module  -*- lexical-binding: t -*-

;; Copyright © 2017 Vasantha Ganesh K. <vasanthaganesh.k@tuta.io>

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


(require 'ert)
(require 'inf-haskell)
(require 'haskell-string)

(ert-deftest test-run-haskell ()
  (run-haskell)
  (should (equal (inferior-haskell-get-result "1 + 1")
                 "2")))

(ert-deftest test-inferior-haskell-buffer ()
  "Check if the inferior haskell buffer has been started"
  (run-haskell)
  (should inferior-haskell-buffer))

(ert-deftest test-inferior-haskell-root-dir ()
  "Check if the root dir of the loaded file/project is not nil
This way we test is the file is loaded or not"
  (run-haskell)
  (should inferior-haskell-root-dir))

;; (ert-deftest test-load-haskell ()
;;   (run-haskell)
;;   (with-temp-file "abc.hs"
;;     (insert "
;; import Data.Array

;; main = interact $ show.fib.read

;; fib n = memo ! n
;;   where memo = listArray (1, n) (map myfib [1..n])
;;         myfib n
;;           | n <= 2 = 1
;;           | otherwise = memo ! (n-1) + memo ! (n-2)
;; ")
;;     (save-buffer)
;;     (inferior-haskell-load-file))
;;   (with-temp-buffer
;;       (set-buffer "*Messages*")
;;       (let ((load-str (car (last (haskell-string-split-to-lines (haskell-string-chomp (buffer-string)))))))
;;         (message (prin1-to-string load-str))
;;         (should (and (string-prefix-p "Loading" load-str)
;;                      (string-suffix-p "abc.hs" load-str))))))
