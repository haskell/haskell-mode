(require 'ert)
(require 'haskell-collapse)

(defun test-haskell-collapse-start-end (start end)
  (let ((start (save-excursion
		 (beginning-of-buffer)
		 (forward-line start)
		 (end-of-line)
		 (point)))
	(end (save-excursion
	       (beginning-of-buffer)
	       (forward-line end)
	       (end-of-line)
	       (point))))
    (cons start end)))

(defun test-haskell-indented-block (source lines result)
  "takes args source (source-code)"
  (with-temp-buffer
    (insert source)
    (beginning-of-buffer)
    (forward-line lines)
    (equal (funcall result)
	   (haskell-indented-block))))

(ert-deftest test-haskell-indented-block-1 ()
    (should (test-haskell-indented-block
"instance FromJSON BlogConfig where
    parseJSON (Object v) = BlogConfig <$>
                           v .: \"blogname\" <*>
                           v .: \"tagline\" <*>
                           v .: \"author\" <*>
                           v .: \"email\" <*>
                           v .: \"twitter\" <*>
                           v .: \"gitlab\" <*>
                           v .: \"github\"
                           
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _ = error \"Can't parse BlogConfig from YAML\"
"
					    1
					    (lambda () (test-haskell-collapse-start-end 1 9)))))
