EMACS = emacs

ELFILES = \
	haskell-font-lock.el \
	haskell-mode.el \
	haskell-doc.el \
	haskell-decl-scan.el \
	haskell-indent.el

ELCFILES = $(ELFILES:.el=.elc)
AUTOLOADS = haskell-site-file.el

%.elc: %.el
	$(EMACS) --batch --eval '(setq load-path (cons "." load-path))' \
		-f batch-byte-compile $<

all: $(ELCFILES) $(AUTOLOADS)

$(AUTOLOADS): $(ELFILES)
	$(EMACS) --batch --eval '(setq generated-autoload-file "'`pwd`'/$@")' -f batch-update-autoloads "."
