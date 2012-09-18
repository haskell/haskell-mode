;;; haskell-session-virthualenv.el -- Haskell sessions.

;; Copyright (C) 2012 Ben Ford

;; Author: Ben Ford <ben.fordnz@gmail.com>

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

;;; Todo:

;;; Code:

(eval-when-compile (require 'cl))
(require 'haskell-cabal)
(require 'haskell-string)

(setq virthualenv nil)
(setq virthualenv-path-backup nil)
(setq virthualenv-exec-path-backup nil)


(defun haskell-sve-find-dir ()
  "Return a buffer visiting the cabal file of the current directory, or nil."
  (catch 'found
    (let ((root (abbreviate-file-name default-directory))
          ve)
      (while root
        (if (setq ve (car (directory-files root 'full "\\.virthualenv\\'")))
            (throw 'found root)
          (if (equal root
                     (setq root (file-name-directory
                                 (directory-file-name root))))
              (setq root nil))))
      nil)))


(defun haskell-sve-read-file (fpath)
  (with-temp-buffer
    (insert-file-contents fpath)
    (buffer-string)))


(defun haskell-sve-activate (dir)
  "Activate the Virtual Haskell Environment in DIR"
  (haskell-sve-deactivate)
  (setq dir (file-name-as-directory dir))

  (let* ((virthualenv-dir (concat dir ".virthualenv/"))
         (path-var-prependix-location (concat virthualenv-dir "path_var_prependix"))
         (ghc-package-path-var-location (concat virthualenv-dir "ghc_package_path_var"))
         (path-var-prependix (virthualenv-read-file path-var-prependix-location))
         (ghc-package-path-var (virthualenv-read-file ghc-package-path-var-location))
         (new-path-var (concat path-var-prependix ":" (getenv "PATH")))
         (exec-path-prependix (split-string path-var-prependix ":")))
    (setq virthualenv-path-backup (getenv "PATH"))
    (setenv "PATH" new-path-var)
    (setq virthualenv-exec-path-backup exec-path)
    (setq exec-path (append exec-path-prependix exec-path))
    (setenv "GHC_PACKAGE_PATH" ghc-package-path-var)
    (setq virthualenv dir)))


(defun haskell-sve-deactivate ()
  "Deactivate the Virtual Haskell Environment"
  (if virthualenv
      (progn
        (setenv "PATH" virthualenv-path-backup)
        (setq exec-path virthualenv-exec-path-backup)
        (setenv "GHC_PACKAGE_PATH" nil)
        (setq virthualenv nil)
        (setq virthualenv-path-backup nil)
        (setq virthualenv-exec-path-backup nil))))

;;;###autoload
(defun haskell-session-set-virthualenv (s v)
  "Set the sessions virthualenv directory"
  (haskell-session-set s 'virthualenv v))

;;;###autoload
(defun haskell-session-virthualenv (s)
  "Get the sessions virthualenv directory"
  (haskell-session-get s 'virthualenv))

;;;###autoload
(defun haskell-virthualenv-get-dir ()
  "Get the .virthualenv dir for a new project. Various ways of figuring this out,
   and indeed just prompting the user. Do them all."
  (let* ((dir (haskell-sve-find-dir)))
    (read-from-minibuffer
     (format "directory containing virthualenv%s: " (if dir (format " (%s)" (file-relative-name dir)) ""))
     (or dir default-directory))))

;;;###autoload
(defun haskell-virthualenv-activate ()
  "Activate the virthualenv for this session"
  (interactive)
  (let* ((s (haskell-session))
         (ve (haskell-session-virthualenv s)))
    (if ve
        (virthualenv-activate ve))))

;;;###autoload
(defun haskell-virthualenv-name (s)
  "Get the name of the currently active virthualenv

   If there isn't one active retruns empty string"
  (if virthualenv
      (file-name-nondirectory (directory-file-name virthualenv))
    ""))

(provide 'haskell-session-virthualenv)
