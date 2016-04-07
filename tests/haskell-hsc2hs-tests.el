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

(defvar fake-hsc2hs "#!/usr/bin/awk -f

/^#{/ {
    skip = 1
}

!skip && !/^#include/ {
    lines = lines $0\"\\n\"
}

/}/ {
    skip = 0
}

/A_TYPO/ {
    print FILENAME\":\"NR\":58: error: ‘A_TYPO’ undeclared (first use in this function)\" >\"/dev/stderr\"
    lines=\"\"
    exit(1)
}

END {
    if(lines) {
        lines = lines \"rand_max  :: NUMBERS\\n\"
        lines = lines \"rand_max  = NUMBERS 2147483647\\n\"
        hs = FILENAME
        sub(/hsc$/, \"hs\", hs)
        if(FILENAME==hs) {
            print FILENAME\" doesn't seem to end in .hsc\">\"/dev/stderr\"
            exit(1)
        }
        else {
            print lines > hs
        }
    }
}
" "Very stupid fake hsc2hs specific to our tests")


(defmacro with-hsc2hs (contents &rest body)
  "Load CONTENTS as a .hsc, then run BODY after it's loaded into REPL.
Uses fake hsc2hs script from this directory."
  (declare (debug t) (indent 1))
  `(with-temp-switch-to-buffer
     (let* ((hsc (make-temp-file "haskell-hsc2hs-tests.el" nil ".hsc"))
            (hs (replace-regexp-in-string "\\.hsc\\'" ".hs" hsc)))
       (insert ,contents)
       (write-file hsc)
       (haskell-mode)
       (with-script-path haskell-process-path-hsc2hs fake-hsc2hs 'keep
         (haskell-process-load-file)
         (let ((proc (get-buffer-process "*hsc2hs*")))
           (while (and proc (eq (process-status proc) 'run)) ; TODO: is there no built-in way to block-wait on a process?
             (sit-for 0.5))
           ,@body)
         (delete-file haskell-process-path-hsc2hs))
       (delete-file hsc)
       (when (file-exists-p hs)
         (delete-file hs)))))

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
  (kill-buffer "*haskell*")
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

