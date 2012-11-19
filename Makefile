bin/markdown: bin/markdown.hs Markdown.hs
	ghc -Wall -fno-warn-unused-do-bind --make ${GHCOPTS} -o $@ $<

.PHONY: clean test prof

test:
	make -C tests --quiet clean all

prof: bin/markdown-prof

bin/markdown-prof:
	ghc --make -auto-all -prof -o $@ bin/markdown.hs

clean:
	-@rm *.o *.hi bin/markdown bin/markdown-prof bin/*.o bin/*.hi; \
	  make -C tests clean
