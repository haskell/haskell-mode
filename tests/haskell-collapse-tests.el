(require 'ert)
(require 'haskell-collapse)

(setq haskell-code-block-1 "instance FromJSON BlogConfig where
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
")

(setq haskell-code-block-2 "--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid
import           Hakyll.Web.Sass (sassCompiler)
import           Hakyll
import           Control.Applicative
import           Data.Yaml
import           Data.Maybe
import qualified Data.ByteString.Char8 as BS

--------------------------------------------------------------------------------
")

(setq haskell-code-block-3 "archiveCtx posts  blogconfig = listField \"posts\" (postCtx blogconfig) (return posts) <>
                               constField \"title\" \"Archive\"                          <>
                               defaultCTX blogconfig



indexCtx posts blogconfig =  listField \"posts\" (postCtx blogconfig) (return (take 5 posts)) <>
                             constField \"title\" \"Posts\"                                     <>
                             defaultCTX blogconfig
")

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
	   haskell-code-block-1
	   1
	   (lambda () (test-haskell-collapse-start-end 1 9)))))

(ert-deftest test-haskell-indented-block-2 ()
  (should (test-haskell-indented-block
	   haskell-code-block-1
	   0
	   (lambda () (test-haskell-collapse-start-end 0 11)))))


(ert-deftest test-haskell-indented-block-3 ()
  (should (test-haskell-indented-block
	   haskell-code-block-1
	   2
	   (lambda () (test-haskell-collapse-start-end 1 9)))))

(ert-deftest test-haskell-indented-block-4 ()
  (should (test-haskell-indented-block
	   haskell-code-block-2
	   0
	   (lambda () nil))))
	   
(ert-deftest test-haskell-indented-block-5 ()
  (should (test-haskell-indented-block
	   haskell-code-block-2
	   1
	   (lambda () nil))))

(ert-deftest test-haskell-indented-block-6 ()
  (should (test-haskell-indented-block
	   haskell-code-block-2
	   3
	   (lambda () nil))))

(ert-deftest test-haskell-indented-block-7 ()
  (should (test-haskell-indented-block
	   haskell-code-block-3
	   0
	   (lambda () (test-haskell-collapse-start-end 0 5)))))
