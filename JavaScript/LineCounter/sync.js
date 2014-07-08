'use strict';

var fs = require('fs');
var path = require('path');
//------------------------------

function countLines(fpath, result) {
    var stat = fs.statSync(fpath);
    if (stat.isDirectory()) {
        var files = fs.readdirSync(fpath);
        for (var i = files.length - 1; i >= 0; --i) {
            countLines(path.join(fpath, files[i]), result);
        }
    } else {
        result.push({path:fpath, lines:fs.readFileSync(fpath).toString('utf-8').split('\n').length });
    }
}


var result = [];
for (var i = process.argv.length - 1; i >= 2; --i) {
    countLines(process.argv[i], result);
}

var totalLines = 0;
for (var i = result.length - 1; i >= 0; --i) {
    console.log(result[i].path, ' => ', result[i].lines);
    totalLines += result[i].lines;
}
console.log('totals', ' => ', totalLines);
