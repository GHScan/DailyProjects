'use strict';

var utils = require('./utils.js');
//------------------------------

(function() {
    var N = 1024 * 1024 * 1024;

    utils.timeIt('forward loop ' + N, function() {
        var sum = 0;
        for (var i = 0; i < N; ++i) sum += i;
    });

    utils.timeIt('backward loop ' + N, function() {
        var sum = 0;
        for (var i = N - 1; i >= 0; --i) sum += i;
    });

})();

//------------------------------
(function() {
    var a = new Array(1024 * 1024);
    for (var i = 0; i < a.length; ++i) a[i] = i;

    utils.timeIt('forward iterate array ' + a.length, function() {
        var sum = 0;
        for (var i = 0; i < a.length; ++i) sum += a[i];
    });

    utils.timeIt('backward iterate array ' + a.length, function() {
        var sum = 0;
        for (var i = a.length - 1; i >= 0; --i) sum += a[i];
    });

    utils.timeIt('iterate array with for-in ' + a.length, function() {
        var sum = 0;
        for (var i in a) sum += a[i];
    });

})();


