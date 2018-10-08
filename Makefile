CASK ?= cask
EMACS ?= emacs
EMACS_BATCH = ${CASK} exec ${EMACS} -Q --batch -l package

REPO = github.com/techniumlabs/ecloud

CASKDIR = .cask
SRCS = $(wildcard *.el)
TARGETS = $(SRCS:.el=.elc)

.PHONY: test

build: $(TARGETS)

$(TARGETS) : $(SRCS) $(CASKDIR)
	${CASK} clean-elc
	${CASK} build

test : $(SRCS)
	${CASK} clean-elc
	${CASK} exec ert-runner --debug --verbose
	${CASK} exec buttercup -L . test/specs

$(CASKDIR) :
	${CASK} install
