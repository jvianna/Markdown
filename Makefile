Markdown: Markdown.hs
	ghc -Wall -fno-warn-unused-do-bind --make ${GHCOPTS} -o $@ $<

.PHONY: clean test

test:
	./shtest -t -p ./Markdown tests/Markdown/

clean:
	rm *.o *.hi Markdown
