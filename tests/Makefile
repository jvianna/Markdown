TESTDIR ?= *
TESTS=$(wildcard $(TESTDIR)/*.markdown)
DIFFS=$(patsubst %.markdown,%.diff,$(TESTS))
PROG ?= ../bin/markdown

all: $(DIFFS)
	PASS=0;TESTS=0; \
	for f in $(DIFFS); do \
	  let TESTS=TESTS+1; \
	  [ -s $$f ] || let PASS=PASS+1; \
	done; \
	echo "\x1b[1m$$PASS of $$TESTS tests passed.\x1b[0m"; \
	if [ $$TESTS -eq $$PASS ]; then exit 0; else exit 1; fi

%.actual.html: %.markdown $(PROG)
	$(PROG) $< > $@

%.diff: %.html %.actual.html
	diff -u $+ > $@ ; \
	  if [ -s $@ ]; then \
	  	echo "\x1b[1;31m✘ $(patsubst %.diff,%,$@)\x1b[0m"; \
		echo; cat $@; echo; \
	  else \
	  	echo "\x1b[1;32m✓ $(patsubst %.diff,%,$@)\x1b[0m"; \
	  fi

.PHONY: all clean

clean:
	-@rm $(DIFFS)
