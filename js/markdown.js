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
                 var res = textLines.join('\n');
                 textLines = [];
                 if (reEmpty.test(res)){
                   return null
                  } else {
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
    while (more) {
	var bscanners = state.blockScanners();
	var lns = state.peekLines();
	var thisLine = lns[0];
	var nextLine = lns[1] || "";
	var remainder = applyScanners(bscanners, thisLine);
	if (remainder == null) {
	    if (continuation) {
              continuation = parseTextLine(thisLine);
            };
            break;
	} else {
	    parseTextLine(state, remainder);
	};
	more = state.advance();
    }
    var lastpara = state.popTextLines();
    if (lastpara){
      xs.push(lastpara);
    };
    return xs;
};

function parseTextLine(state, str) {
  var lscanners = state.lineScanners();
  var remainder = applyScanners(lscanners, str);
  if (remainder == null || reEmpty.test(remainder)) {
    return false;
  } else {
    state.addTextLine(str);
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


