CASK ?= cask
EMACS ?= emacs
EMACS_BATCH = ${CASK} exec ${EMACS} -Q --batch -l package

REPO = github.com/techniumlabs/ecloud

CASKDIR = .cask
SRCS = $(wildcard *.el)
TARGETS = $(SRCS:.el=.elc)

.PHONY: test

$(TARGETS) : $(SRCS) $(CASKDIR)
	${CASK} clean-elc
	${CASK} build

test : $(SRCS)
	${CASK} clean-elc
	${CASK} exec ert-runner

$(CASKDIR) :
	${CASK} install
