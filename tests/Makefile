TESTDIR ?= *
TESTS=$(wildcard $(TESTDIR)/*.markdown)
DIFFS=$(patsubst %.markdown,%.diff,$(TESTS))
PROG ?= ../bin/markdown
TIDYCMD ?= tidy -asxhtml -utf8 --show-body-only yes --show-warnings no -quiet

all: $(DIFFS)
	PASS=0;TESTS=0; \
	for f in $(DIFFS); do \
	  let TESTS=TESTS+1; \
	  [ -s $$f ] || let PASS=PASS+1; \
	done; \
	echo -e "\033[1m$$PASS of $$TESTS tests passed.\033[0m"; \
	if [ $$TESTS -eq $$PASS ]; then exit 0; else exit 1; fi

%.actual.html: %.markdown $(PROG)
ifeq ($(TIDY),1)
	-$(PROG) $< | $(TIDYCMD) > $@
else
	$(PROG) $< > $@
endif

%.expected.html: %.html
ifeq ($(TIDY),1)
	-$(TIDYCMD) $< > $@
else
	cp $< $@
endif

%.diff: %.expected.html %.actual.html
	diff --unified=1 $+ > $@ ; \
	  if [ -s $@ ]; then \
		echo -e "\033[1;31m✘ $(patsubst %.diff,%,$@)\033[0m"; \
		echo -e "\033[0;36m" ; cat $@; echo -e "\033[0m"; \
	  else \
		echo -e "\033[1;32m✓ $(patsubst %.diff,%,$@)\033[0m"; \
	  fi

.PHONY: all clean

clean:
	-@rm $(DIFFS)
