Markdown
========

This is an experimental markdown parser in Haskell.  It is five times as fast
as pandoc asd uses a sixth of the memory.  It is also more forgiving:  it
allows blockquotes, headers, indented code blocks, horizontal rules, and lists
to start without a preceding blank line.  And it is more flexible about the
indentation of sublists.

Several markdown extensions are implemented, including fenced code
blocks and significant list start numbers.  All URLs are made into
hyperlinks.

It will be open sourced in due time.

Copyright &copy; 2012 John MacFarlane.

