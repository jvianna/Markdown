Markdown
========

This is an experimental markdown parser in Haskell.  It is seven times faster
than pandoc (about the same as the C library peg-markdown) and uses a fifth of
the memory.  It is also more forgiving:  it allows blockquotes, headers,
indented code blocks, horizontal rules, and lists to start without a preceding
blank line.  And it is more flexible about the indentation of sublists.

Several markdown extensions are implemented, including fenced code blocks and
significant list start numbers.  All URLs are made into hyperlinks.
See comments in the code for more details.

To compile, get the Haskell Platform, then install dependencies:

    cabal update && cabal install blaze-html attoparsec

Then 'make'.  'make test' will run some tests.

The library will be open sourced in due time.

Copyright &copy; 2012 John MacFarlane.

