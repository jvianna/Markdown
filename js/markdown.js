var fs = require('fs');
var assert = require('assert');
var util = require('util');

// partly based on the version here: http://stackoverflow.com/questions/7343796/markdown-showdown-bug-in-detab-regex
function detab_line(text) {
    var spaces = ["    ", "   ", "  ", " "], skew = 0, v;
    return text.replace(/\t/g, function (match, offset) {
        v = (offset - skew) % 4;
        skew = offset + 1;
        return spaces[v];
    });
}
assert.equal(detab_line("\thi\tthere \tfriend"), "    hi  there   friend");

// for now, just print ast
function toHtml(x) {
    return(util.inspect(x,false,null));
}

function parserState(str) {
    var inputRemaining = str;
    var blockScanners = [];
    var lineScanners = [];
    var linkReferences = {};
    var textLines = [];
    function normalizeLabel(str) {
	return str.replace(/[\n ]+/g,' ').toUpperCase();
    };
    return { peekLines : function() {
                 return inputRemaining.split(/\n/,2);
             },
             advance : function() {
                 var i = inputRemaining.indexOf('\n');
                 if (i == -1) {
                     return false;
                 } else {
                     inputRemaining = inputRemaining.slice(i + 1);
                     return true;
                 };
             },
	     blockScanners : function() {
		 return blockScanners;
	     },
	     lineScanners : function() {
		 return lineScanners;
	     },
	     pushBlockScanner : function(scanner) {
		 blockScanners.push(scanner);
		 return true;
	     },
	     pushLineScanner : function(scanner) {
		 lineScanners.push(scanner);
		 return true;
	     },
	     popBlockScanner : function(scanner) {
		 return blockScanners.pop();
             },
	     popLineScanner : function(scanner) {
		 return lineScanners.pop();
	     },
	     addLinkReference : function(label, url, title) {
		 linkReferences[normalizeLabel(label)] =
                   { url: url,
                     title: title
                   };
		 return true;
	     },
	     lookupLinkReference : function(label) {
		 return linkReferences[normalizeLabel(label)];
             },
             addTextLine : function(str) {
                 textLines.push(str);
                 return true;
             },
             popTextLines : function() {
                 if (textLines.length == 0) {
                   return null;
                 } else {
                   var res = textLines.join('\n');
                   textLines = [];
     	  	   return { t:'Para',
                            v: [{t: 'Markdown',v: res}]
                          };
                 };
	     }
    }
}

// scanners

scanNonidentSpaces = new RegExp('^ {0,3}');
scanIndentSpace = new RegExp('^    ');
scanBlockquoteStart = new RegExp('^ {0,3}> ?');
optScanBlockquoteStart = new RegExp('^( {0,3}> ?)?');
scanBlankline = new RegExp('^ *$');
scanSpace = new RegExp('^ ');
scanSpaces = new RegExp('^ *');

reEmpty = new RegExp('^[ \n\t\r]*$');

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

function parseLines(state, continuation, line){
    var xs = [];
    var more = true;
    var continuation = false;
    var last;
    while (more) {
	var bscanners = state.blockScanners();
	var lns = state.peekLines();
	var thisLine = lns[0];
	var nextLine = lns[1] || "";
	var remainder = applyScanners(bscanners, thisLine);
	if (remainder == null) {
            // TODO - why did we need continuation again?
	    if (continuation) {
              continuation = parseTextLine(state, thisLine);
            };
            break;
	} else {
            // here is where we check for new blocks
            for (i in scanners) {
              var s = scanners[i];
              if (s.scanner.test(remainder)) {
                last = state.popTextLines();
                if (last) { xs.push(last); }
                xs.push(s.parser(state, remainder));
              } else if (parseTextLine(state, remainder)) {
                continuation = true;
                break;
              } else {
                last = state.popTextLines();
                if (last) { xs.push(last); }
              };
            };
	};
	more = state.advance();
    }
    last = state.popTextLines();
    if (last){ xs.push(last); };
    return xs;
};

function getLines(state, ln) {
  state.advance()
  var lscanners = state.lineScanners()
  var lns = [ln];
  var more = state.peekLines();
  while (more.length > 0) {
    var res = applyScanners(lscanners, more[0]);
    if (res) {
      lns.push(res);
      state.advance();
      more = state.peekLines();
    } else {
      break;
    }
  }
  return lns;
}

var scanners = [
  { scanner: scanBlockquoteStart,
    parser:  pBlockquote }
];

function pBlockquote(state) {
  state.pushBlockScanner(scanBlockquoteStart);
  state.pushLineScanner(optScanBlockquoteStart);
  var res = parseLines(state);
  state.popLineScanner();
  state.popBlockScanner();
  return { t: 'Blockquote', v: res };
};

function parseTextLine(state, str) {
  var lscanners = state.lineScanners();
  var remainder = applyScanners(lscanners, str);
  if (remainder == null || scanBlankline.test(remainder)) {
    return false;
  } else { // TODO add this part to Markdown.hs
    for (i in scanners) {
      var s = scanners[i];
      if (s.scanner.test(remainder)) {
        return false;
      };
    }
    state.addTextLine(remainder);
    return true;
  };
}

function parseMarkdown(inputString) {
    var state = parserState(inputString);
    // state.pushBlockScanner(scanBlockquoteStart);
    return parseLines(state, false);
}

// main program


var inputFile = process.argv[2] || '/dev/stdin';

var contents = fs.readFileSync(inputFile,'utf8');

console.log(toHtml(parseMarkdown(contents)));


