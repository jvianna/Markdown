TESTS=$(wildcard *.markdown */*.markdown)
ACTUALS=$(patsubst %.markdown,%.actual.html,$(TESTS))
PROG ?= ../bin/markdown

all: $(ACTUALS)
	PASS=0;TESTS=0; \
	for f in $(ACTUALS); do \
	  let TESTS=TESTS+1; \
	  diff -u $${f%.actual.html}.html $$f ; \
	  [ $$? -eq 0 ] && let PASS=PASS+1 && echo "✓ $${f%.actual.html}" || \
	  echo "✘ $${f%.actual.html}"; \
	done; \
	echo "$$PASS of $$TESTS tests passed."; \
	if [ $$TESTS -eq $$PASS ]; then exit 0; else exit 1; fi

%.actual.html: %.markdown $(PROG)
	$(PROG) $< > $@

.PHONY: all clean

clean:
	-@rm $(ACTUALS)
