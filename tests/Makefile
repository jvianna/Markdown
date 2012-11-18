TESTS=$(wildcard *.markdown */*.markdown)
DIFFS=$(patsubst %.markdown,%.diff,$(TESTS))
PROG ?= ../bin/markdown

all: $(DIFFS)
	PASS=0;TESTS=0; \
	for f in $(ACTUALS); do \
	  let TESTS=TESTS+1; \
	  if ![ -s $$f ]; then PASS=PASS+1; fi ;\
	done; \
	echo "$$PASS of $$TESTS tests passed."; \
	if [ $$TESTS -eq $$PASS ]; then exit 0; else exit 1; fi

%.actual.html: %.markdown $(PROG)
	$(PROG) $< > $@

%.diff: %.html %.actual.html
	diff -u $+ > $@ ; \
	  if [ -s $@ ]; then \
	  	echo "✘ $(patsubst %.diff,%,$@)"; \
		cat $@; \
	  else \
	  	echo "✓ $(patsubst %.diff,%,$@)"; \
	  fi

.PHONY: all clean

clean:
	-@rm $(DIFFS)
