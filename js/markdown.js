var fs = require('fs');
var assert = require('assert');
var util = require('util');

// Utility functions

// partly based on the version here:
// http://stackoverflow.com/questions/7343796/markdown-showdown-bug-in-detab-regex
function detab_line(text) {
    var spaces = ["    ", "   ", "  ", " "], skew = 0, v;
    return text.replace(/\t/g, function (match, offset) {
        v = (offset - skew) % 4;
        skew = offset + 1;
        return spaces[v];
    });
}

assert.equal(detab_line("\thi\tthere \tfriend"), "    hi  there   friend");

// link labels are case-insensitive and collapse whitespace
function normalizeLabel(str) {
    return str.replace(/[\n ]+/g,' ').toUpperCase();
};

assert.equal(normalizeLabel("По\n оживлённым берегам"), normalizeLabel("ПО ОЖИВЛЁННЫМ БЕРЕГАМ"));

// scanners

scanNonidentSpaces = new RegExp('^ {0,3}');
scanIndentSpace = new RegExp('^    ');
scanIndentSpaceOrBlankline = new RegExp('^    |^ *$');
scanBlockquoteStart = new RegExp('^ {0,3}> ?');
optScanBlockquoteStart = new RegExp('^( {0,3}> ?)?');
scanBlankline = new RegExp('^ *$');
scanSpace = new RegExp('^ ');
scanSpaces = new RegExp('^ *');
scanHrule = new RegExp('^ {0,3}(([*] *){3,}|(- *){3,}|(_ *){3,}) *$');
scanWhitespace = new RegExp('^[ \n\t]+$');

function applyScanners(scanners, str) {
    var i;
    var s = str;
    for (i in scanners) {
	var res = scanners[i].exec(s);
	if (res) {
	    s = s.slice(res[0].length);
	} else {
	    return null;
	};
    };
    return s; // should be the remainder of the string
}

assert.equal(applyScanners([],"hi"),"hi");
assert.equal(applyScanners([scanBlockquoteStart],"> hi"), "hi");
assert.equal(applyScanners([scanBlockquoteStart,scanBlockquoteStart]," >> hi"), "hi");


function Markdown(input){

    var markdown = new Object();

    markdown.inputRemaining = input; // input string
    markdown.blockScanners = [];
    markdown.lineScanners = [];
    markdown.linkReferences = {};
    markdown.textLines = [];

    var firsttwo = input.split(/\n/,2);

    // thisLine and nextLine are always strings; at the
    // end of the document they become empty strings.
    markdown.thisLine = detab_line(firsttwo[0] || "");
    markdown.nextLine = detab_line(firsttwo[1] || "");

    // returns true if it succeeded in advancing, false
    // at end of document.
    markdown.advance = function() {
	if (this.inputRemaining) {
	    this.thisLine = this.nextLine;
            var i = this.inputRemaining.indexOf('\n');
            if (i == -1) {
		this.nextLine = "";
		this.inputRemaining = "";
            } else {
		this.inputRemaining = this.inputRemaining.slice(i + 1);
		this.nextLine = detab_line(this.inputRemaining.split(/\n/,2)[1] || "");
            }
	    return true;
	} else {
	    return false;
	}
    }

    markdown.addLinkReference = function(label, url, title) {
	this.linkReferences[normalizeLabel(label)] =
            { url: url,
              title: title
            };
	return true;
    }

    markdown.lookupLinkReference = function(label) {
	return this.linkReferences[normalizeLabel(label)];
    }

    markdown.addTextLine = function(str) {
        this.textLines.push(str);
        return true;
    }

    // empty the textLines buffer into a new paragraph.
    markdown.popTextLines = function(blocks) {
        if (this.textLines.length > 0) {
            var res = this.textLines.join('\n');
            this.textLines = [];
     	    blocks.push({ t:'Para', raw: res});
        };
    }

    // for now, just print ast
    markdown.showAST = function() {
	var blocks = this.parseBlocks();
	this.processBlocks(blocks);
	return(util.inspect(blocks,false,null));
    }

    markdown.toHtml = function() {
	var blocks = this.parseBlocks();
	this.processBlocks(blocks);
	return blocksToHtml(blocks);
    }

    var escapeHtml = function(x) {
	return x.replace(/[&<>'"]/,
			 function(c){
			     if (c=='&') {
				 return "&amp;";
			     } else if (c=='<') {
				 return "&lt;";
			     } else if (c=='>') {
				 return "&gt;";
			     } else if (c=='\'') {
				 return "&#39;";
			     } else if (c=='"') {
				 return "&quot;";
			     } else {
				 return c;
			     }
			 });
    }

    var blocksToHtml = function(blocks) {
	var xs = [];
	for (i in blocks) {
	    var block = blocks[i];
	    switch (block.t){
	    case 'Para':
		xs.push("<p>" + inlinesToHtml(block.v) + "</p>");
		break;
	    case 'Blockquote':
		xs.push("<blockquote>\n" + blocksToHtml(block.v) + "\n</blockquote>");
		break;
	    case 'CodeBlock':
		xs.push("<pre><code>" + escapeHtml(block.v) + "</code></pre>");
		break;
	    default:
	    }
	}
	return xs.join("\n");
    }

    var inlinesToHtml = function(inlines) {
	var xs = [];
	for (i in inlines) {
	    var inline = inlines[i];
	    switch (inline.t){
	    case 'Str':
		xs.push(escapeHtml(inline.v));
		break;
	    case 'Space':
		xs.push(" ");
		break;
	    default:
	    }
	}
	return xs.join("\n");
    }

    // Modifies blocks in place, parsing 'raw' strings into
    // arrays of inlines and resolving references.
    markdown.processBlocks = function(blocks) {
	for (i in blocks) {
	    var block = blocks[i];
	    switch (block.t) {
	    case 'Para':
		if (block.raw) {
		    block.v = this.parseInlines(block.raw);
		    delete block.raw;
		}
		break;
	    case 'Blockquote':
		this.processBlocks(block.v);
		break;
	    default:
	    }
	}
    }

    // parse a string into an array of inline objects,
    // resolving references.
    markdown.parseInlines = function(str) {
	// for testing purposes
	var inlines = [];
	var words = str.split(/\b/);
	for (i in words) {
	    if (scanWhitespace.test(words[i])) {
		inlines.push({t:'Space'});
	    } else {
		inlines.push({t:'Str',v:words[i]});
	    }
	}
	return inlines;
    }

    // reads lines and returns an array of block elements.
    // stops when it can't parse any more blocks. this may
    // be at the end of the document or at the end of a block
    // container such as a blockquote or list item.
    markdown.parseBlocks = function() {
	var blocks = [];
	var more = true;
	var last;
	var continuation = false;
	while (more) {
	    // try applying the blockscanners to the first line.
	    var remainder = applyScanners(this.blockScanners, this.thisLine);
	    if (remainder == null) {
		// failed:  we either have a lazy text continuation line
		// or we're done.
		if (continuation) {
		    continuation = this.parseTextLine(this.thisLine);
		    more = continuation && this.advance();
		} else {
		    break;
		}
	    } else {
		// success: see what kind of block we have
		var found = false;
		for (i in this.scanners) {
		    var s = this.scanners[i];
		    var matched;
		    if (!found && (matched = s.scanner.exec(remainder))) {
			this.popTextLines(blocks);
			blocks.push(s.parser(this,
					     remainder,
					     remainder.slice(matched[0].length)));
			found = true;
		    }
		}
		if (!found) {
		    // nothing matched; check first for blank line
		    if (scanBlankline.test(remainder)) {
			var nxt = applyScanners(this.blockScanners, this.nextLine);
			if (nxt != null && scanBlankline.test(nxt) && this.blockScanners.length > 0) {
			    // two blank lines break out of a block container
			    // such as a list item.  we know we're in a block
			    // container if blockScanners.length > 0.
			    more = this.advance();
			    break;
			} else {
			    // paragraph break.  pop paragraph lines and
			    // move on.
			    this.popTextLines(blocks);
			    more = this.advance();
			}
		    } else {
			// if non-blank, just parse it as a text line
			// and move on.
			continuation = this.parseTextLine(remainder);
			more = this.advance();
		    }
		}
	    };
	}
	this.popTextLines(blocks);
	return blocks;
    }

    // add a non-blank line to the buffer of paragraph lines.
    markdown.parseTextLine = function(str) {
	var remainder = applyScanners(this.lineScanners, str);
	if (remainder == null || scanBlankline.test(remainder)) {
	    return false;
	} else {
	    for (i in this.scanners) {
		var s = this.scanners[i];
		if (s.scanner.test(remainder)) {
		    return false;
		};
	    }
	    this.addTextLine(remainder);
	    return true;
	};
    }

    // get lines while scanners match, removing matched portions.
    // return an array of lines.
    markdown.getLines = function(scanners) {
	var lns = [];
	more = this.advance()
	while (more) {
	    var res = applyScanners(scanners, this.thisLine);
	    if (res) {
		lns.push(res);
		more = this.advance();
	    } else {
		break;
	    }
	}
	return lns;
    }

    var pBlockquote = function(m) {
	m.blockScanners.push(scanBlockquoteStart);
	m.lineScanners.push(optScanBlockquoteStart);
	var res = m.parseBlocks();
	m.lineScanners.pop();
	m.blockScanners.pop();
	return { t: 'Blockquote', v: res };
    }

    var pIndentedCodeBlock = function(m,_,t) {
	m.blockScanners.push(scanIndentSpaceOrBlankline);
	var rest = m.getLines(m.blockScanners);
   	var code = (t + "\n" + rest.join('\n')).replace(/\n*$/,'\n');
	m.blockScanners.pop();
	return { t: 'CodeBlock', attr: { codeLang: '' }, v: code };
    }

    var pHrule = function(m) {
	m.advance();
	return { t: 'HRule' };
    }

    markdown.scanners = [
	{ scanner: scanBlockquoteStart,
	  parser:  pBlockquote },
	{ scanner: scanIndentSpace,
	  parser:  pIndentedCodeBlock },
	{ scanner: scanHrule,
	  parser:  pHrule },
    ];

    return markdown;
}




// main program


var inputFile = process.argv[2] || '/dev/stdin';
var contents = fs.readFileSync(inputFile,'utf8');
var markdown = new Markdown(contents);
console.log(markdown.toHtml());

