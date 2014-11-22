'use strict'; 

//------------------------------
function timeit(times, f) {
    if (times > 1) f();

    var start = new Date();
    for (var i = 0; i < times; ++i) f();
    console.log((new Date() - start) / times);
}
//------------------------------
function filter(newa, a, f) {
    newa.length = 0;
    for (var i = 0; i < a.length; ++i) {
        if (f(a[i])) newa.push(a[i]);
    }
    return newa;
}

function map(newa, a, f) {
    newa.length = 0;
    for (var i = 0; i < a.length; ++i) {
        newa[i] = f(a[i]);
    }
    return newa;
}

function reduce(a, f, init) {
    for (var i = 0; i < a.length; ++i) init = f(a[i], init);
    return init;
}
//------------------------------
var filterCache = {}
function filter2(newa, a, fs) {
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

var mapCache = {}
function map2(newa, a, fs) {
    var f = mapCache[fs];
    if (!f) {
        var template = '(function(newa, a){for (var i = 0; i < a.length; ++i) newa.push(@(a[i]));})';
        var str = template.replace(/@\(([^\)]+)\)/g, fs);
        f = eval(str);
        mapCache[fs] = f;
    }
    newa.length = 0;
    f(newa, a);
    return newa;
}

var reduceCache = {}
function reduce2(a, fs, init) {
    var f = reduceCache[fs];
    if (!f) {
        var template = '(function(a, init){ for (var i = 0; i < a.length; ++i) init = @(init, a[i]); return init;})';
        var str = template.replace(/@\(([^,]+),([^\)]+)\)/g, fs);
        f = eval(str);
        reduceCache[fs] = f;
    }
    return f(a, init);
}
//------------------------------
var LEN = 1024 * 256;
var TIMES = 10;

var a = [];
for (var i = 0; i < LEN; ++i) a.push(i);
var newa = new Array(a.length);

console.log('#################');
timeit(TIMES, function() {
    a.filter(function(i){return (i&1)==1;});
});
timeit(TIMES, function() {
    a.map(function(v){ return v + v;});
});
timeit(TIMES, function() {
    a.reduce(function(i, j){ return i + j; }, 0);
});

console.log('#################');
timeit(TIMES, function() {
    filter(newa, a, function(i){return (i&1)==1;});
});
timeit(TIMES, function() {
    map(newa, a, function(v){ return v + v;});
});
timeit(TIMES, function() {
    reduce(a, function(i, j){ return i + j; }, 0);
});

console.log('#################');
timeit(TIMES, function() {
    filter2(newa, a, '($1&1)==1');
});
timeit(TIMES, function() {
    map2(newa, a, '$1+$1');
});
timeit(TIMES, function() {
    reduce2(a, '$1+$2', 0);
});
