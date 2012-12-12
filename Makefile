EMACS = emacs
BATCH = $(EMACS) --batch -Q

ELFILES = \
	haskell-c.el \
	haskell-cabal.el \
	haskell-decl-scan.el \
	haskell-doc.el \
	haskell-font-lock.el \
	haskell-ghci.el \
	haskell-hugs.el \
	haskell-indent.el \
	haskell-indentation.el \
	haskell-checkers.el \
	haskell-mode.el \
	haskell-simple-indent.el \
	haskell-sort-imports.el \
	haskell-align-imports.el \
	haskell-move-nested.el \
	haskell-navigate-imports.el \
	haskell-interactive-mode.el \
	haskell-package.el \
	haskell-process.el \
	haskell-session.el \
	haskell-string.el \
	ghc-core.el \
	inf-haskell.el

ELCFILES = $(ELFILES:.el=.elc)
# AUTOLOADS = $(PACKAGE)-startup.el
AUTOLOADS = haskell-site-file.el

%.elc: %.el
	$(BATCH) --eval '(setq load-path (cons "." load-path))' \
		-f batch-byte-compile $<

.PHONY: all compile info dist clean

all: $(AUTOLOADS)

compile: $(ELCFILES)

clean:
	$(RM) $(ELCFILES)

info:
	# No Texinfo file, sorry.

######################################################################
###                    don't look below                            ###
######################################################################

PACKAGE=haskell-mode

$(AUTOLOADS): $(ELFILES)
	[ -f $@ ] || echo '' >$@
	$(BATCH) --eval '(setq generated-autoload-file "'`pwd`'/$@")' -f batch-update-autoloads "."

##

VERSION = $(shell darcs show tags | head -n 1)
TAG = $(shell echo v$(VERSION) | sed 's/\./\\\./g')
TMP = $(shell echo $(PACKAGE)-$(VERSION))

dist:
	darcs get --lazy . $(TMP) &&\
	cd $(TMP) &&\
	rm -r _darcs &&\
	sed -i 's/\$$Name:  \$$/$(TAG)/g' * &&\
	make $(AUTOLOADS) &&\
	rm *~ &&\
	darcs changes > ChangeLog &&\
	rm Makefile &&\
	cd .. &&\
	tar czf $(PACKAGE)-$(VERSION).tar.gz $(PACKAGE)-$(VERSION) &&\
	rm -rf $(PACKAGE)-$(VERSION) &&\
	mv $(PACKAGE)-$(VERSION).tar.gz ../haskellmode-emacs-web/
