#
# Note: Due to MELPA distributing directly from github source version
# needs to be embedded in files as is without preprocessing.
#
# Version string is present in:
# - Makefile
# - haskell-mode.el
# - haskell-mode.texi
#
# We should have a script that changes it everywhere it is needed and
# syncs it with current git tag.
#
VERSION = 16.2-git

INSTALL_INFO = install-info

# Use $EMACS environment variable if present, so that all of these are
# equivalent:
#
# 1.  export EMACS=/path/to/emacs && make
# 2.  EMACS=/path/to/emacs make
# 3.  make EMACS=/path/to/emacs
#
# This is particularly useful when EMACS is set in ~/.bash_profile
#
EMACS := $(shell which "$${EMACS}" 2> /dev/null || which "emacs" 2> /dev/null)
EMACS_VERSION := $(shell "$(EMACS)" -Q --batch --eval '(princ emacs-version)')

EFLAGS = --eval "(add-to-list 'load-path (expand-file-name \"tests/compat\") 'append)" \
	 --eval "(when (boundp 'load-prefer-newer) (setq load-prefer-newer t))"

BATCH = @echo EMACS $@; $(EMACS) $(EFLAGS) --batch -Q -L .

ELFILES := $(filter-out haskell-mode-autoloads.el haskell-mode-pkg.el,$(wildcard *.el))

ELCHECKS := $(wildcard tests/*-tests.el)

AUTOLOADS = haskell-mode-autoloads.el

.PHONY: all compile clean check check-emacs-version

all: check-emacs-version compile $(AUTOLOADS)

check-emacs-version :
	$(BATCH) --eval "(when (version< emacs-version \"24.3\")				\
                            (message \"Error: haskell-mode requires Emacs 24.3 or later\")	\
                            (message \"Your version of Emacs is %s\" emacs-version)		\
                            (message \"Found as '$(EMACS)'\")					\
                            (message \"Use one of:\")						\
                            (message \"   1.  export EMACS=/path/to/emacs && make\")		\
                            (message \"   2.  EMACS=/path/to/emacs make\")			\
                            (message \"   3.  make EMACS=/path/to/emacs\")			\
                            (kill-emacs 2))"
	@echo Using EMACS = $(EMACS), version = $(EMACS_VERSION)

compile: build-$(EMACS_VERSION)/build-flag

build-$(EMACS_VERSION) :
	mkdir $@

# Emacs byte compilation state leaks from file to file if multiple
# files are requested to be build at the same time. We have to
# workaround this issue on Makefile level. Note also that we consider
# an .el file to be dependent on all other files because we do not do
# proper dependency tracking (yet).
build-$(EMACS_VERSION)/%.elc : %.el $(ELFILES)
	$(BATCH) --eval '(setq byte-compile-error-on-warn t)'						\
	         --eval "(defun byte-compile-dest-file (filename)					\
	               	       (concat (file-name-directory filename) \"build-\" emacs-version \"/\"	\
	                      	    (file-name-nondirectory filename) \"c\"))"				\
	         --eval "(when (check-declare-file \"$<\") (kill-emacs 2))" \
	         -f batch-byte-compile $<								\

build-$(EMACS_VERSION)/build-flag : build-$(EMACS_VERSION) $(patsubst %.el,build-$(EMACS_VERSION)/%.elc,$(ELFILES))
	touch $@

check-%: tests/%-tests.el
	$(BATCH) -l "$<" -f ert-run-tests-batch-and-exit;

check: compile $(AUTOLOADS) check-ert

check-ert: $(ELCHECKS)
	$(BATCH) --eval "(when (= emacs-major-version 24)					\
                           (require 'undercover)						\
                           (undercover \"*.el\"							\
                              (:exclude \"haskell-mode-pkg.el\" \"haskell-compat.el\")))"	\
                 -L tests									\
                 $(patsubst %,-l %,$(ELCHECKS))							\
                 -f ert-run-tests-batch-and-exit
	@echo "checks passed!"

clean:
	$(RM) -r build-$(EMACS_VERSION) $(AUTOLOADS) $(AUTOLOADS:.el=.elc)

$(AUTOLOADS): $(ELFILES)
	$(BATCH) \
		--eval '(setq make-backup-files nil)' \
		--eval "(setq generated-autoload-file (concat command-line-default-directory \"/\" \"$@\"))" \
		-f batch-update-autoloads "."
# check if autoloads will really load
	$(BATCH) -l "$@"

check-external : check-emacs-version $(AUTOLOADS)
	$(BATCH) -l tests/haskell-external.el                                           \
                 -f haskell-check-external-batch-and-exit
	@echo "external packages okay"
