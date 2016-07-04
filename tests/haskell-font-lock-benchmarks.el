;; haskell-font-lock-benchmarks.el --- -*- lexical-binding: t; -*-

(require 'haskell-mode)

(defun memory-allocation-of-function (symbol log)
  "Get memory allocation stats for SYMBOL from LOG.

SYMBOL is a function name to find and LOG comes from
`profiler-memory-log'. Return sum of all allocations done in the
call graph starting with function."
  (let ((stat 0))
    (maphash (lambda (key val)
               ;; `key' is a vector of symbols from the top of the
               ;; stack, it looks like this:
               ;;
               ;; [sort zonk command-line-1 command-line normal-top-level nil nil ...]
               ;;
               ;; We are interested in any stacks that mention the
               ;; function in question because it means either the
               ;; function itself or any of the functions called
               ;; allocated memory. Sum all of it.

               ;;(message "%S: %S" key val)
               (setq stat (+ stat (catch 'return
                                    (mapc (lambda (item)
                                            (unless item
                                              ;; cut short at the first nil, there
                                              ;; is nothing interesting past that
                                              ;; point
                                              (throw 'return 0))

                                            (when (equal item symbol)
                                              ;; found the function
                                              ;;(message "%S: %d" key val)
                                              (throw 'return val))) key)
                                    0))))
             log)
    stat))

(ert-deftest haskell-font-lock-allocation ()
  (with-temp-buffer
    (haskell-mode)
    (insert-file-contents
     (expand-file-name "test-data/Data-Map-Base.hs"
                       (file-name-directory
                        (or (symbol-file 'haskell-font-lock-allocation)
                            (buffer-file-name)))))
    (profiler-memory-start)
    (font-lock-ensure)
    (let* ((log (profiler-memory-log))
           (font-lock-ensure-allocation
            (memory-allocation-of-function 'font-lock-ensure log)))
      (should (>= 3462728 font-lock-ensure-allocation))
      (profiler-memory-stop))))
