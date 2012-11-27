PROG ?= bin/markdown

bin/markdown: bin/markdown.hs Markdown.hs
	ghc -Wall -fno-warn-unused-do-bind --make ${GHCOPTS} -o $@ $<

.PHONY: clean test prof opt bench

test: $(PROG)
	make -C tests --quiet clean all

prof: bin/markdown-prof

opt:  bin/markdown-opt

bench: bin/markdown-opt
	time $< tests/Original/Markdown_Documentation_Syntax.markdown >/dev/null
	time ../peg-markdown/markdown tests/Original/Markdown_Documentation_Syntax.markdown >/dev/null

bin/markdown-prof: bin/markdown.hs Markdown.hs
	ghc --make -rtsopts -auto-all -prof -o $@ bin/markdown.hs

bin/markdown-opt: bin/markdown.hs Markdown.hs
	ghc --make -fforce-recomp -O2 -o $@ bin/markdown.hs

clean:
	-@rm *.o *.hi bin/markdown bin/markdown-prof bin/markdown-opt \
	  bin/*.o bin/*.hi; \
	  make -C tests clean
