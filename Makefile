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

test : $(SRCS)
	${CASK} clean-elc
	${CASK} exec buttercup -L . test/specs

$(CASKDIR) :
	${CASK} install
