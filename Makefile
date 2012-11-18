bin/markdown: bin/markdown.hs Markdown.hs
	ghc -Wall -fno-warn-unused-do-bind --make ${GHCOPTS} -o $@ $<

.PHONY: clean test

test:
	make -C tests --quiet clean all

clean:
	-rm *.o *.hi Markdown bin/markdown bin/*.o bin/*.hi; \
	  -@make -C tests clean
