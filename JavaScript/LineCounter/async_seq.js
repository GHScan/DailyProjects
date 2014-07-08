'use strict';

var fs = require('fs');
var path = require('path');
//------------------------------

function countLines(fpath, result, k) {
    fs.stat(fpath, function(err, stat) {
        if (stat.isDirectory()) {
            fs.readdir(fpath, function(err, files) {
                var i = files.length - 1;
                (function f() {
                    if (i < 0) {
                        k();
                    } else {
                        countLines(path.join(fpath, files[i--]), result, f);
                    }
                })();
            });
        } else {
            fs.readFile(fpath, function(err, content) {
                result.push({path:fpath, lines:content.toString('utf-8').split('\n').length});
                k();
            });
        }
    });
}

var argi = process.argv.length - 1;
var result = [];
(function f() {
    if (argi >= 2) {
        countLines(process.argv[argi--], result, f);
    } else {
        var totalLines = 0;
        for (var i = result.length - 1; i >= 0; --i) {
            console.log(result[i].path, ' => ', result[i].lines);
            totalLines += result[i].lines;
        }
        console.log('total', ' => ', totalLines);
    }
})();
