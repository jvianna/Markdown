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
scanBlockquoteStart = new RegExp('^ {0,3}> ?');
optScanBlockquoteStart = new RegExp('^( {0,3}> ?)?');
scanBlankline = new RegExp('^ *$');
scanSpace = new RegExp('^ ');
scanSpaces = new RegExp('^ *');

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
    markdown.thisLine = detab_line(firsttwo[0] || "");
    markdown.nextLine = detab_line(firsttwo[1] || "");

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

    markdown.popTextLines = function(blocks) {
        if (this.textLines.length > 0) {
            var res = this.textLines.join('\n');
            this.textLines = [];
     	    blocks.push({ t:'Para',
			  v: [{t: 'Markdown',v: res}]
			});
        };
    }

    // for now, just print ast
    markdown.showAST = function() {
	return(util.inspect(this.parseBlocks(),false,null));
    }

    markdown.parseBlocks = function() {
	var blocks = [];
	var more = true;
	var last;
	var continuation = false;
	while (more) {
	    var remainder = applyScanners(this.blockScanners, this.thisLine);
	    if (remainder == null) {
		if (continuation) {
		    continuation = this.parseTextLine(this.thisLine);
		    more = this.advance();
		} else {
		    break;
		}
	    } else {
		// here is where we check for new blocks
		var found = false;
		for (i in this.scanners) {
		    var s = this.scanners[i];
		    if (!found && s.scanner.test(remainder)) {
			this.popTextLines(blocks);
			blocks.push(s.parser(this,remainder));
			found = true;
		    }
		}
		if (!found) {
		    if (scanBlankline.test(remainder)) {
			var nxt = applyScanners(this.blockScanners, this.nextLine);
			if (nxt != null && scanBlankline.test(nxt) && this.blockScanners.length > 0) {
			    more = this.advance();
			    break;
			} else {
			    this.popTextLines(blocks);
			    more = this.advance();
			}
		    } else {
			continuation = this.parseTextLine(remainder);
			more = this.advance();
		    }
		}
	    };
	}
	this.popTextLines(blocks);
	return blocks;
    }

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

    markdown.getLines = function(ln) {
	var lns = [ln];
	more = this.advance()
	while (more) {
	    var res = applyScanners(this.lineScanners, this.thisLine);
	    if (res) {
		lns.push(res);
		more = this.advance();
	    } else {
		break;
	    }
	}
	return lns;
    }

    pBlockquote = function(m) {
	m.blockScanners.push(scanBlockquoteStart);
	m.lineScanners.push(optScanBlockquoteStart);
	var res = m.parseBlocks();
	m.lineScanners.pop();
	m.blockScanners.pop();
	return { t: 'Blockquote', v: res };
    }

    markdown.scanners = [
	{ scanner: scanBlockquoteStart,
	  parser:  pBlockquote }
    ];

    return markdown;
}




// main program


var inputFile = process.argv[2] || '/dev/stdin';
var contents = fs.readFileSync(inputFile,'utf8');
var markdown = new Markdown(contents);
console.log(markdown.showAST());

