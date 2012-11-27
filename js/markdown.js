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
		 linkReferences[normalizeLabel(label)] = { url: url, title: title };
		 return true;
	     },
	     lookupLinkReference : function(label) {
		 return linkReferences[normalizeLabel(label)];
             },
    }
}

// scanners

scanNonidentSpaces = /^ {0,3}/;
scanIndentSpace = /^    /;
scanBlockquoteStart = /^ {0,3}> ?/;
scanBlankline = /^ *$/;
scanSpace = /^ /;
scanSpaces = /^ */;



function parseMarkdown(inputString) {
    var state = parserState(inputString);
    var next;
    var xs = [];
    var more = true;
    while (more) {
	var lns = state.peekLines();
	xs.push({t: 'Markdown', v: lns[0]});
	more = state.advance();
    }
    return([{t: 'Para', v: xs}]);
}

// main program


var inputFile = process.argv[2] || '/dev/stdin';

var contents = fs.readFileSync(inputFile,'utf8');

console.log(toHtml(parseMarkdown(contents)));


