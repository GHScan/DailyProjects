'use strict'; 

//------------------------------
function timeit(times, f) {
    if (times > 1) f();

    var start = new Date();
    for (var i = 0; i < times; ++i) f();
    console.log((new Date() - start) / times);
}

//------------------------------
var filterCache = {}
function filter(newa, a, fs) {
    var f = filterCache[fs];
    if (!f) {
        var template = '(function(newa, a){for (var i = 0; i < a.length; ++i) { if (@(a[i])) newa.push(a[i]); }})';
        var str = template.replace(/@\(([^\)]+)\)/g, fs);
        f = eval(str);
        filterCache[fs] = f;
    }
    newa.length = 0;
    f(newa, a);
    return newa;
}

function map(newa, a, fs) {
    var f = filterCache[fs];
    if (!f) {
        var template = '(function(newa, a){for (var i = 0; i < a.length; ++i) newa.push(@(a[i]));})';
        var str = template.replace(/@\(([^\)]+)\)/g, fs);
        f = eval(str);
        filterCache[fs] = f;
    }
    newa.length = 0;
    f(newa, a);
    return newa;
}

function reduce(a, fs, init) {
    var f = filterCache[fs];
    if (!f) {
        var template = '(function(a, init){var v = init; for (var i = 0; i < a.length; ++i) v = @(v, a[i]); return v;})';
        var str = template.replace(/@\(([^,]+),([^\)]+)\)/g, fs);
        f = eval(str);
        filterCache[fs] = f;
    }
    return f(a, init);
}
//------------------------------

var a = [];
for (var i = 0; i < 1024 * 256; ++i) a.push(i);
var newa = new Array(a.length);

timeit(10, function() {
    a.filter(function(v){ return v & 1 == 1; });
});
timeit(10, function() {
    a.map(function(v){ return v + v; });
});
timeit(10, function() {
    a.reduce(function(a, b){ return a + b; }, 0);
});

timeit(10, function() {
    filter(newa, a, '$1&1==1');
});
timeit(10, function() {
    map(newa, a, '$1+$1');
});
timeit(10, function() {
    reduce(a, '$1+$2', 0);
});
