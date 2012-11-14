Markdown: Markdown.hs
	ghc -Wall -fno-warn-unused-do-bind --make ${GHCOPTS} -o $@ $<

.PHONY: clean test

test:
	make -C tests --quiet

clean:
	-rm *.o *.hi Markdown; \
	  make -C tests clean
