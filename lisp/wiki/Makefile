# TODO: grab these off of the wiki when there are updates (!!!!) :3

EMACS ?=emacs
EMACS_FLAGS=-Q -batch --no-init -f batch-byte-compile

TARGETS=rotate-text.elc linkd.elc

compile: $(TARGETS)

all: clean compile

%.elc: %.el
	$(EMACS) $(EMACS_FLAGS) $^

clean:
	rm -f *.elc

