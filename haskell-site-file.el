;;; haskell-site-file.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path
              (or (file-name-directory load-file-name) (car load-path)))


;;;### (autoloads (haskell-doc-show-type haskell-doc-current-info
;;;;;;  turn-off-haskell-doc-mode turn-on-haskell-doc-mode haskell-doc-mode)
;;;;;;  "haskell-doc" "haskell-doc.el" (16749 45058))
;;; Generated autoloads from haskell-doc.el

(autoload (quote haskell-doc-mode) "haskell-doc" "\
Enter `haskell-doc-mode' for showing fct types in the echo area.
See variable docstring.

\(fn &optional PREFIX)" t nil)

(autoload (quote turn-on-haskell-doc-mode) "haskell-doc" "\
Unequivocally turn on `haskell-doc-mode' (see variable documentation).

\(fn)" t nil)

(autoload (quote turn-off-haskell-doc-mode) "haskell-doc" "\
Unequivocally turn off `haskell-doc-mode' (see variable documentation).

\(fn)" t nil)

(autoload (quote haskell-doc-current-info) "haskell-doc" "\
Return the info about symbol at point.
Meant for `eldoc-print-current-symbol-info-function'.

\(fn)" nil nil)

(autoload (quote haskell-doc-show-type) "haskell-doc" "\
Show the type of the function near point.
For the function under point, show the type in the echo area.
This information is extracted from the `haskell-doc-prelude-types' alist
of prelude functions and their types, or from the local functions in the
current buffer.

\(fn &optional SYM)" t nil)

;;;***

;;;### (autoloads (literate-haskell-mode haskell-mode) "haskell-mode"
;;;;;;  "haskell-mode.el" (16750 63485))
;;; Generated autoloads from haskell-mode.el

(autoload (quote haskell-mode) "haskell-mode" "\
Major mode for editing Haskell programs.  Last adapted for Haskell 1.4.
Blank lines separate paragraphs, comments start with `-- '.

\\<haskell-mode-map>\\[indent-for-comment] will place a comment at an appropriate place on the current line.
\\[comment-region] comments (or with prefix arg, uncomments) each line in the region.

Literate scripts are supported via `literate-haskell-mode'.  The
variable `haskell-literate' indicates the style of the script in the
current buffer.  See the documentation on this variable for more
details.

Modules can hook in via `haskell-mode-hook'.  The following modules
are supported with an `autoload' command:

   `haskell-font-lock', Graeme E Moss and Tommy Thorn
     Fontifies standard Haskell keywords, symbols, functions, etc.

   `haskell-decl-scan', Graeme E Moss
     Scans top-level declarations, and places them in a menu.

   `haskell-doc', Hans-Wolfgang Loidl
     Echoes types of functions or syntax of keywords when the cursor is idle.

   `haskell-indent', Guy Lapalme
     Intelligent semi-automatic indentation.

   `haskell-simple-indent', Graeme E Moss and Heribert Schuetz
     Simple indentation.

   `haskell-hugs', Guy Lapalme
     Interaction with Hugs interpreter.

Module X is activated using the command `turn-on-X'.  For example,
`haskell-font-lock' is activated using `turn-on-haskell-font-lock'.
For more information on a module, see the help for its `turn-on-X'
function.  Some modules can be deactivated using `turn-off-X'.  (Note
that `haskell-doc' is irregular in using `turn-(on/off)-haskell-doc-mode'.)

Use `haskell-version' to find out what version this is.

Invokes `haskell-mode-hook' if not nil.

\(fn)" t nil)

(autoload (quote literate-haskell-mode) "haskell-mode" "\
As `haskell-mode' but for literate scripts.

\(fn)" t nil)
(add-to-list 'auto-mode-alist '("\\.\\(?:[gh]s\\|hi\\)\\'" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.l[gh]s\\'" . literate-haskell-mode))
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;;;***

;;;### (autoloads nil nil ("haskell-decl-scan.el" "haskell-font-lock.el"
;;;;;;  "haskell-ghci.el" "haskell-hugs.el" "haskell-indent.el" "haskell-simple-indent.el")
;;;;;;  (16750 64679 383330))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; haskell-site-file.el ends here
