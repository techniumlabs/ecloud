CASK ?= cask
EMACS ?= emacs
EMACS_BATCH = ${CASK} exec ${EMACS} -Q --batch -l package

REPO = github.com/techniumlabs/ecloud

CASKDIR = .cask
SRCS = $(wildcard *.el)
TARGETS = $(SRCS:.el=.elc)

LINTELS = $(filter-out ecloud-autoloads.el,$(SRCS))

.PHONY: test

build: $(TARGETS)

clean:
	${CASK} clean-elc

$(TARGETS) : $(SRCS) $(CASKDIR)
	${CASK} clean-elc
	${CASK} build

lint:
	${CASK} exec $(EMACS) -Q --batch \
		--eval "(setq enable-local-variables :safe)" \
		-l elisp-lint.el -f elisp-lint-files-batch \
		--no-package-format \
                --no-fill-column \
		$(LINTELS)

package-lint:
	${CASK} exec $(EMACS) -Q --batch \
		--eval "(require 'package)" \
		--eval "(push '(\"melpa\" . \"http://melpa.org/packages/\") package-archives)" \
		--eval "(package-initialize)" \
		--eval "(package-refresh-contents)" \
		-l package-lint.el -f package-lint-batch-and-exit \
    ecloud.el

test : lint package-lint
	${CASK} clean-elc
	${CASK} exec buttercup -L . --debug

$(CASKDIR) :
	${CASK} install
