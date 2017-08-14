;;; interactive-haskell-mode-tests.el --- Tests for Haskell Interactive Mode  -*- lexical-binding: t -*-

;; Copyright © 2016 Athur Fayzrakhmanov. All rights reserved.

;; This file is part of haskell-mode package.
;; You can contact the authors using GitHub issue tracker:
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

;; This package provides regression tests for the package
;; haskell-interactive-mode.

;;; Code:


(require 'ert)
(require 'haskell-test-utils)

(ert-deftest haskell-interactive-error-regexp-test-1 ()
  "Tests the regexp `haskell-interactive-mode-error-regexp'"
  (with-temp-buffer
    (insert "/home/user/Test.hs:24:30:")
    (goto-char (point-min))
    (should (haskell-compile-error-p))))

(ert-deftest haskell-interactive-error-regexp-test-2 ()
  "Tests the regexp `haskell-interactive-mode-error-regexp'"
  (with-temp-buffer
    (insert " Test.hs:8:9:")
    (goto-char (point-min))
    (should (not (haskell-compile-error-p)))))

(ert-deftest haskell-interactive-error-regexp-test-3 ()
  "Tests the regexp `haskell-interactive-mode-error-regexp'"
  (with-temp-buffer
    (insert "Test.hs:5:18:")
    (goto-char (point-min))
    (should (haskell-compile-error-p))))

(ert-deftest haskell-interactive-error-regexp-test-4 ()
  "Tests the regexp `haskell-interactive-mode-error-regexp'"
  (with-temp-buffer
    (insert "Test.hs:7:6: Not in scope: type constructor or class ‘Ty’")
    (goto-char (point-min))
    (should (haskell-compile-error-p))))

(ert-deftest haskell-interactive-error-regexp-test-5 ()
  "Tests the regexp `haskell-interactive-mode-error-regexp'"
  (with-temp-buffer
    (insert "Test.hs:9:5: Not in scope: ‘c’")
    (goto-char (point-min))
    (should (haskell-compile-error-p))))

(ert-deftest test-haskell-process-load-file ()
  (haskell-unconditional-kill-buffer "*haskell-compilation*")
  (haskell-unconditional-kill-buffer "*haskell*")
  (message (format "Default dir: %s" default-directory))
  (find-file-literally (concat default-directory
                               (file-name-as-directory "tests")
                               (file-name-as-directory "sample-code")
                               "fibonoacci.hs"))
  (with-current-buffer "fibonoacci.hs"
    (haskell-mode)
    (haskell-process-load-file)
    (should (buffer-live-p (get-buffer "*haskell-compilation*")))))

(ert-deftest test-haskell-process-load-file-fail ()
  (haskell-unconditional-kill-buffer "*haskell-compilation*")
  (haskell-unconditional-kill-buffer "*haskell*")
  (find-file-literally (concat default-directory
                               (file-name-as-directory "tests")
                               (file-name-as-directory "sample-code")
                               "does_not_compile.hs"))
  (with-current-buffer "does_not_compile.hs"
    (haskell-mode)
    (haskell-process-load-file)
    (with-current-buffer "*haskell-compilation*"
      (goto-char (point-min))
      (should (haskell-compile-error-p)))))
