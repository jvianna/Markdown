TESTS=$(wildcard *.markdown */*.markdown)
ACTUALS=$(patsubst %.markdown,%.actual.html,$(TESTS))
PROG ?= ../bin/markdown

all: $(ACTUALS)
	for f in $(ACTUALS); do \
	  diff -u $${f%.actual.html}.html $$f ; \
	  [ $$? -eq 0 ] && echo "✓ $${f%.actual.html}" ; \
	done

%.actual.html: %.markdown $(PROG)
	$(PROG) $< > $@

.PHONY: all clean

clean:
	-@rm $(ACTUALS)
