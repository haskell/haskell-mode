;;; inf-haskell-mode.el --- Interaction with an inferior Haskell process -*- lexical-binding: t -*-

;; Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009  Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords: Haskell

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

;;; Commentary:

;; A minor mode for interactive editing in source buffers.

(require 'inf-haskell)
(require 'haskell-mode)

;;;###autoload
(defun inferior-haskell-send-decl ()
  "Send current declaration to inferior-haskell process."
  (interactive)
  (save-excursion
    (goto-char (1+ (point)))
    (let* ((proc (inferior-haskell-process))
           (start (or (haskell-ds-backward-decl) (point-min)))
           (end (or (haskell-ds-forward-decl) (point-max)))
           (raw-decl (buffer-substring start end)))
      ;; enter multiline-prompt-cutting-mode
      (with-current-buffer (process-buffer proc)
        (setq inferior-haskell-send-decl-post-filter-on t))
      ;; flash decl
      (inferior-haskell-flash-decl start end)
      ;; send decl
      (comint-send-string proc (inferior-haskell-wrap-decl raw-decl))
      ;; send preview
      (inferior-haskell-send-command
       proc
       (let* ((str (remove ?\n raw-decl))
              (len (min 15 (length str))))
         (concat "-- evaluating {: "
                 (substring str 0 len)
                 (if (= 15 len) ".." "")
                 " :}"))))))

;;;###autoload
(defun inferior-haskell-type (expr &optional insert-value)
  "Query the haskell process for the type of the given expression.
If optional argument `insert-value' is non-nil, insert the type above point
in the buffer.  This can be done interactively with the \\[universal-argument] prefix.
The returned info is cached for reuse by `haskell-doc-mode'."
  (interactive
   (let ((sym (haskell-ident-at-point)))
     (list (read-string (if sym
                            (format "Show type of (default %s): " sym)
                          "Show type of: ")
                        nil nil sym)
           current-prefix-arg)))
  (if (string-match "\\`\\s_+\\'" expr) (setq expr (concat "(" expr ")")))
  (let ((type (inferior-haskell-get-result (concat ":type " expr))))
    (if (not (string-match (concat "^\\(" (regexp-quote expr)
                                   "[ \t\n]+\\(::\\|∷\\)[ \t\n]*\\(.\\|\n\\)*\\)")
                           type))
        (error "No type info: %s" type)
      (progn
        (setf type (match-string 1 type))
        ;; Cache for reuse by haskell-doc.
        (when (and (boundp 'haskell-doc-mode) haskell-doc-mode
                   (boundp 'haskell-doc-user-defined-ids)
                   ;; Haskell-doc only works for idents, not arbitrary expr.
                   (string-match "\\`(?\\(\\s_+\\|\\(\\sw\\|\\s'\\)+\\)?[ \t]*\\(::\\|∷\\)[ \t]*"
                                 type))
          (let ((sym (match-string 1 type)))
            (setq haskell-doc-user-defined-ids
                  (cons (cons sym (substring type (match-end 0)))
                        (delq (assoc sym haskell-doc-user-defined-ids)
                              haskell-doc-user-defined-ids)))))

        (if (called-interactively-p 'any) (message "%s" type))
        (when insert-value
          (beginning-of-line)
          (insert type "\n"))
        type))))

;;;###autoload
(defun inferior-haskell-kind (type)
  "Query the haskell process for the kind of the given expression."
  (interactive
   (let ((type (haskell-ident-at-point)))
     (list (read-string (if type
                            (format "Show kind of (default %s): " type)
                          "Show kind of: ")
                        nil nil type))))
  (let ((result (inferior-haskell-get-result (concat ":kind " type))))
    (if (called-interactively-p 'any) (message "%s" result))
    result))

;;;###autoload
(defun inferior-haskell-find-definition (sym)
  "Attempt to locate and jump to the definition of the given expression."
  (interactive
   (let ((sym (haskell-ident-at-point)))
     (list (read-string (if sym
                            (format "Find definition of (default %s): " sym)
                          "Find definition of: ")
                        nil nil sym))))
  (let ((info (inferior-haskell-info sym)))
    (if (not (string-match inferior-haskell-info-xref-re info))
        (error "No source information available")
      (let ((file (match-string-no-properties 1 info))
            (line (string-to-number
                   (match-string-no-properties 2 info)))
            (col (string-to-number
                  (match-string-no-properties 3 info))))
        (when file
          (with-current-buffer (process-buffer (inferior-haskell-process))
            ;; The file name is relative to the process's cwd.
            (setq file (expand-file-name file)))
          ;; Push current location marker on the ring used by `find-tag'
          (require 'etags)
          (xref-push-marker-stack)
          (pop-to-buffer (find-file-noselect file))
          (when line
            (goto-char (point-min))
            (forward-line (1- line))
            (when col (move-to-column col))))))))

;;;###autoload
(defun inferior-haskell-find-haddock (sym)
  "Find and open the Haddock documentation of SYM.
Make sure to load the file into GHCi or Hugs first by using C-c C-l.
Only works for functions in a package installed with ghc-pkg, or
whatever the value of `haskell-package-manager-name' is.

This function needs to find which package a given module belongs
to.  In order to do this, it computes a module-to-package lookup
alist, which is expensive to compute (it takes upwards of five
seconds with more than about thirty installed packages).  As a
result, we cache it across sessions using the cache file
referenced by `inferior-haskell-module-alist-file'. We test to
see if this is newer than `haskell-package-conf-file' every time
we load it."
  (interactive
   (let ((sym (haskell-ident-at-point)))
     (list (read-string (if sym
                            (format "Find documentation of (default %s): " sym)
                          "Find documentation of: ")
                        nil nil sym))))
  (let* (;; Find the module and look it up in the alist
         (module (inferior-haskell-get-module sym))
         (full-name (inferior-haskell-map-internal-ghc-ident (concat module "." sym)))
         (_success (string-match "\\(.*\\)\\.\\(.*\\)" full-name))
         (module (match-string 1 full-name))
         (sym (match-string 2 full-name))
         (alist-record (assoc module (inferior-haskell-module-alist)))
         (package (nth 1 alist-record))
         (file-name (concat (subst-char-in-string ?. ?- module) ".html"))
         (local-path (concat (nth 2 alist-record) "/" file-name))
         (url (if (or (eq inferior-haskell-use-web-docs 'always)
                      (and (not (file-exists-p local-path))
                           (eq inferior-haskell-use-web-docs 'fallback)))
                  (concat inferior-haskell-web-docs-base package "/" file-name)
                (and (file-exists-p local-path)
                     (concat "file://" local-path))))
         ;; Jump to the symbol within Haddock.
         (url (concat url "#v:" sym)))
    (if url (browse-url url) (error "Local file doesn't exist"))))

;;;###autoload
(defun inferior-haskell-load-and-run (command)
  "Pass the current buffer's file to haskell and then run a COMMAND."
  (interactive
   (list
    (if (and inferior-haskell-run-command (not current-prefix-arg))
        inferior-haskell-run-command
      (read-string "Command to run: " nil nil inferior-haskell-run-command))))
  (setq inferior-haskell-run-command command)
  (let* ((inferior-haskell-errors nil)
         (neh (lambda () (setq inferior-haskell-errors t))))
    (unwind-protect
        (let ((inferior-haskell-wait-and-jump t))
          (add-hook 'next-error-hook neh)
          (inferior-haskell-load-file))
      (remove-hook 'next-error-hook neh))
    (unless inferior-haskell-errors
      (inferior-haskell-send-command (inferior-haskell-process) command)
      (switch-to-haskell))))

;;;###autoload
(defun inferior-haskell-load-file (&optional reload)
  "Pass the current buffer's file to the inferior haskell process.
If prefix arg \\[universal-argument] is given, just reload the previous file."
  (interactive "P")
  ;; Save first, so we're sure that `buffer-file-name' is non-nil afterward.
  (save-buffer)
  (let ((buf (current-buffer))
        (file buffer-file-name)
        (proc (inferior-haskell-process)))
    (if file
        (with-current-buffer (process-buffer proc)
          (compilation-forget-errors)
          (let ((parsing-end (marker-position (process-mark proc)))
                root)
            ;; Go to the root of the Cabal project, if applicable.
            (when (and inferior-haskell-find-project-root
                       (setq root (inferior-haskell-find-project-root buf)))
              ;; Not sure if it's useful/needed and if it actually works.
              (unless (equal default-directory root)
                (setq default-directory root)
                (inferior-haskell-send-command
                 proc (concat ":cd " default-directory)))
              (setq file (file-relative-name file)))
            (inferior-haskell-send-command
             proc (if reload ":reload"
                    (concat ":load \""
                            ;; Espace the backslashes that may occur in file names.
                            (replace-regexp-in-string "[\\\"]" "\\\\\&" file)
                            "\"")))
            ;; Move the parsing-end marker *after* sending the command so
            ;; that it doesn't point just to the insertion point.
            ;; Otherwise insertion may move the marker (if done with
            ;; insert-before-markers) and we'd then miss some errors.
            (if (boundp 'compilation-parsing-end)
                (if (markerp compilation-parsing-end)
                    (set-marker compilation-parsing-end parsing-end)
                  (setq compilation-parsing-end parsing-end))))
          (with-selected-window (display-buffer (current-buffer) nil 'visible)
            (goto-char (point-max)))
          ;; Use compilation-auto-jump-to-first-error if available.
          ;; (if (and (boundp 'compilation-auto-jump-to-first-error)
          ;;          compilation-auto-jump-to-first-error
          ;;          (boundp 'compilation-auto-jump-to-next))
          ;;     (setq compilation-auto-jump-to-next t)
          (when inferior-haskell-wait-and-jump
            (inferior-haskell-wait-for-prompt proc)
            (ignore-errors                  ;Don't beep if there were no errors.
              (next-error))))
      (error "No file associated with buffer"))))

(defun inferior-haskell-reload-file ()
  "Tell the inferior haskell process to reread the current buffer's file."
  (interactive)
  (inferior-haskell-load-file 'reload))

(defvar inf-haskell-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map (kbd "M-C-x")    'inferior-haskell-send-defun)
    ;; (define-key map (kbd "C-x C-e") 'inferior-haskell-send-last-sexp)
    ;; (define-key map (kbd "C-c C-r") 'inferior-haskell-send-region)
    (define-key map (kbd "C-x C-d") 'inferior-haskell-send-decl)
    (define-key map (kbd "C-c C-z") 'switch-to-haskell)
    (define-key map (kbd "C-c C-l") 'inferior-haskell-load-file)
    ;; I think it makes sense to bind inferior-haskell-load-and-run to C-c
    ;; C-r, but since it used to be bound to `reload' until June 2007, I'm
    ;; going to leave it out for now.
    ;; (define-key map (kbd "C-c C-r") 'inferior-haskell-load-and-run)
    (define-key map (kbd "C-c C-b") 'switch-to-haskell)
    ;; (define-key map (kbd "C-c C-s") 'inferior-haskell-start-process)
    ;; That's what M-; is for.
    (define-key map (kbd "C-c C-t") 'inferior-haskell-type)
    (define-key map (kbd "C-c C-i") 'inferior-haskell-info)
    (define-key map (kbd "C-c M-.") 'inferior-haskell-find-definition)
    (define-key map (kbd "C-c C-d") 'inferior-haskell-find-haddock)
    (define-key map (kbd "C-c C-v") 'haskell-check)
    map)
  "Keymap for using `inf-haskell-mode'.")

;;;###autoload
(define-minor-mode inf-haskell-mode
  "Minor mode for enabling inf-haskell process interaction."
  :lighter " Inf-Haskell"
  :keymap inf-haskell-mode-map)

(provide 'inf-haskell-mode)
