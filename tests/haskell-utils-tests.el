;;; haskell-utils-tests.el --- Tests for Haskell utilities package

;; Copyright © 2016 Athur Fayzrakhmanov. All rights reserved.

;; This file is part of haskell-mode package.
;; You can contact with authors using GitHub issue tracker:
;; https://github.com/haskell/haskell-mode/issues

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This package provides regression tests for haskell-utils package.

;;; Code:

(require 'ert)
(require 'haskell-utils)

(defun insert-lines (&rest lines)
  (dolist (line lines)
    (insert (concat line "\n"))))

(ert-deftest simple-import-parse ()
  (should (equal "A.B.C"
                 (with-temp-buffer
                   (insert-lines "import A.B.C")
                   (goto-char (point-min))
                   (forward-line 0)
                   (haskell-utils-parse-import-statement-at-point)))))

(ert-deftest qualified-import-parse ()
  (should (equal "A.B.C"
                 (with-temp-buffer
                   (insert-lines "import qualified A.B.C")
                   (goto-char (point-min))
                   (forward-line 0)
                   (haskell-utils-parse-import-statement-at-point)))))

(ert-deftest qualified-as-import-parse ()
  (should (equal "AAA.Bc.Cx"
                 (with-temp-buffer
                   (insert-lines "import qualified AAA.Bc.Cx as Something")
                   (goto-char (point-min))
                   (forward-line 0)
                   (haskell-utils-parse-import-statement-at-point)))))

(ert-deftest international-characters-import-parse ()
  (should (equal "Żółć"
                 (with-temp-buffer
                   (insert-lines "import Żółć")
                   (goto-char (point-min))
                   (forward-line 0)
                   (haskell-utils-parse-import-statement-at-point)))))

(ert-deftest commented-out-import-parse ()
  (should (equal nil
                 (with-temp-buffer
                   (insert-lines "-- import Nothing")
                   (goto-char (point-min))
                   (forward-line 0)
                   (haskell-utils-parse-import-statement-at-point)))))

(ert-deftest non-import-import-parse ()
  (should (equal nil
                 (with-temp-buffer
                   (insert-lines "something import Nothing")
                   (goto-char (point-min))
                   (forward-line 0)
                   (haskell-utils-parse-import-statement-at-point)))))

(ert-deftest many-spaces-import-parse ()
  (should (equal "M"
                 (with-temp-buffer
                   (insert-lines "\t import\t qualified \t\tM\tas G")
                   (goto-char (point-min))
                   (forward-line 0)
                   (haskell-utils-parse-import-statement-at-point)))))

(ert-deftest using-underscores-import-parse ()
  (should (equal "Module_1.S_3_3_"
                 (with-temp-buffer
                   (insert-lines "import Module_1.S_3_3_")
                   (goto-char (point-min))
                   (forward-line 0)
                   (haskell-utils-parse-import-statement-at-point)))))

(ert-deftest slightly-malformed-import-parse ()
  (should (equal "q.Module...qwerqwe..."
                 (with-temp-buffer
                   (insert-lines "import q.Module...qwerqwe...")
                   (goto-char (point-min))
                   (forward-line 0)
                   (haskell-utils-parse-import-statement-at-point)))))

(ert-deftest package-import-parse ()
  (should (equal "B"
                 (with-temp-buffer
                   (insert-lines "import \"package-1.2.3\" B")
                   (goto-char (point-min))
                   (forward-line 0)
                   (haskell-utils-parse-import-statement-at-point)))))

(ert-deftest safe-haskell-import-parse ()
  (should (equal "B"
                 (with-temp-buffer
                   (insert-lines "import safe B")
                   (goto-char (point-min))
                   (forward-line 0)
                   (haskell-utils-parse-import-statement-at-point)))))

(ert-deftest full-import-parse ()
  (should (equal "Data.Char.Unicode_v_7"
                 (with-temp-buffer
                   (insert-lines "import safe qualified \"unicode-7.0\" Data.Char.Unicode_v_7 as U (func)")
                   (goto-char (point-min))
                   (forward-line 0)
                   (haskell-utils-parse-import-statement-at-point)))))

;;; haskell-utils-tests.el ends here
