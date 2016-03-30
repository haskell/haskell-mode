;; haskell-hsc2hs-tests.el --- -*- lexical-binding: t; -*-

(require 'ert)
(require 'haskell)
(require 'haskell-test-utils)


(defvar default-hsc "{-# LANGUAGE CPP                      #-}
{-# LANGUAGE CApiFFI                  #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Hsc2hsTest where

import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

#include <stdlib.h>

newtype NUMBERS = NUMBERS { unNUMBERS :: CInt }
    deriving (Eq,Show)

#{enum NUMBERS, NUMBERS
 , rand_max = RAND_MAX
 }
")

(defmacro with-hsc2hs (contents &rest body)
  "Load CONTENTS as a .hsc, then run BODY after it's loaded into REPL.
Uses fake hsc2hs script from this directory."
  (declare (debug t) (indent 1))
  `(with-temp-switch-to-buffer
     (let ((f (make-temp-file "haskell-hsc2hs-tests.el" nil ".hsc")))
       (insert ,contents)
       (write-file f)
       (haskell-mode)
       (let* ((dir (file-name-directory
                    (find-lisp-object-file-name 'with-hsc2hs nil)))
              (haskell-process-path-hsc2hs (format "%s/%s" dir "fake-hsc2hs")))
         (haskell-process-load-file))
       (let ((proc (get-buffer-process "*hsc2hs*")))
         (while (eq (process-status proc) 'run) ; TODO: is there no built-in way to block-wait on a process?
           (sit-for 0.5))
         ,@body
         (delete-file f)
         (delete-file (replace-regexp-in-string "\\.hsc\\'" ".hs" f))))))

(ert-deftest hsc2hs-errors ()
  (let ((error-hsc (concat default-hsc
                           "newtype FOO = FOO { unFOO :: CInt } deriving (Eq,Show)\n"
                           "#{enum FOO, FOO , a_typo = A_TYPO }\n")))
    (with-hsc2hs error-hsc
      (with-current-buffer "*hsc2hs*"
        (goto-char (point-min))
        (when (re-search-forward "A_TYPO" nil 'noerror)
          (goto-char (match-beginning 0)))
        (should (looking-at-p "A_TYPO. undeclared"))))))

(ert-deftest hsc2hs-compile-and-load ()
  (with-hsc2hs default-hsc
    (with-current-buffer "*haskell*" ; TODO: Where is this defined?
      (goto-char (point-max))
      (insert ":t unNUMBERS rand_max")
      (goto-char (point-max))
      (haskell-interactive-handle-expr)
      (sit-for 1.0) ; TODO: can we wait until the prompt appears, with a timeout?
      (forward-line -1)
      (should (looking-at-p "unNUMBERS rand_max :: CInt")))))

;; haskell-hsc2hs-tests.el ends here

