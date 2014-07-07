'use strict';

var utils = require('./utils.js');
//------------------------------

function fib(n) {
    return n < 2 ? n : fib(n - 1) + fib(n - 2);
}

utils.timeIt("fib 33", function() {
    fib(33);
});

