'use strict';

var utils = require('./utils.js');
//------------------------------
function _permutationWithCallback(l, off, callback) {
    if (off == l.length) {
        callback(l);
        return;
    }

    var t;
    for (var i = off; i < l.length; ++i) {
        t = l[off]; l[off] = l[i]; l[i] = t;
        _permutationWithCallback(l, off + 1, callback);
        t = l[off]; l[off] = l[i]; l[i] = t;
    }
}

function permutationWithCallback(l, callback) {
    _permutationWithCallback(l, 0, callback);
}
//------------------------------
function nextPermutation(l) {
    var off = l.length - 2;
    while (off >= 0 && l[off] > l[off + 1]) --off;
    if (off < 0) return false;

    var i = off + 1;
    while (l[i] > l[off] && i < l.length) ++i;
    --i;

    var t = l[off]; l[off] = l[i]; l[i] = t;

    var j;
    for (i = off + 1, j = l.length - 1; i < j; ++i, --j) {
        t = l[i]; l[i] = l[j]; l[j] = t;
    }

    return true;
}
//------------------------------
utils.timeIt('permutationWithCallback', function() {
    var l = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
    var count = 0;

    permutationWithCallback(l, function() { ++count;});

    console.log(count);
});

utils.timeIt('nextPermutation', function() {
    var l = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
    var count = 0;
    
    do ++count;
    while(nextPermutation(l));

    console.log(count);
});
