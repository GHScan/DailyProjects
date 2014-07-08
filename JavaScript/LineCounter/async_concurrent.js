'use strict';

var fs = require('fs');
var path = require('path');
//------------------------------
var asyncQueue = {
    _available: 8,
    _pendingRequests: [],
    _nextRequest: function() {
        if (this._pendingRequests.length > 0 && this._available > 0) {
            var last = this._pendingRequests.pop();
            --this._available;
            last();
        }
    },
    request1: function(obj, method, arg1, callback) {
        var This = this;

        this._pendingRequests.push(function() {
            method.call(obj, arg1, function() {
                ++This._available;
                callback.apply(null, arguments);
                This._nextRequest();
            });
        });

        this._nextRequest();
    },
};

//------------------------------
function countLines(fpath, result, k) {
    asyncQueue.request1(fs, fs.stat, fpath, function(err, stat) {
        if (stat.isDirectory()) {
            asyncQueue.request1(fs, fs.readdir, fpath, function(err, files) {
                var succ = 0;
                for (var i = files.length - 1; i >= 0; --i) {
                    countLines(path.join(fpath, files[i]), result, function() {
                        if (++succ == files.length) {
                            k();
                        }
                    });
                }
            });
        } else {
            asyncQueue.request1(fs, fs.readFile, fpath, function(err, content) {
                if (err) throw err;
                result.push({path:fpath, lines:content.toString('utf-8').split('\n').length});
                k();
            });
        }
    });
}

var result = [];
var succ = 2;
for (var i = process.argv.length - 1; i >= 2; --i) {
    countLines(process.argv[i], result, function() {
        if (++succ == process.argv.length) {
            var totalLines = 0;
            for (var i = result.length - 1; i >= 0; --i) {
                console.log(result[i].path, ' => ', result[i].lines);
                totalLines += result[i].lines;
            }
            console.log('total', ' => ', totalLines);
        }
    });
}
