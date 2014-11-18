'use strict'; 

//------------------------------
function timeit(times, f) {
    if (times > 1) f();

    var start = new Date();
    for (var i = 0; i < times; ++i) f();
    console.log((new Date() - start) / times);
}

function swap(a, i, j) {
    var t = a[i];
    a[i] = a[j];
    a[j] = t;
}
//------------------------------
function qsort(a) {
    if (a.length == 0) return a;
    return qsort(a.filter(function(v){return v < a[0];}))
        .concat(a.filter(function(v){ return v == a[0]; }),
                qsort(a.filter(function(v){ return v > a[0];})))
}

function qsort2(a) {
    var newa = a.slice();
    _qsort2(newa, 0, newa.length);
    return newa;
}
function _qsort2(a, begin, end) {
    if (begin == end) return;

    var p = begin;
    for (var i = begin + 1; i < end; ++i) {
        if (a[i] < a[begin]) swap(a, i, ++p);
    }
    swap(a, begin, p);

    _qsort2(a, begin, p);
    _qsort2(a, p + 1, end);
}

function qsort3(a) {
    var newa = a.slice();
    _qsort3(newa, 0, newa.length);
    return newa;
}
function _qsort3(a, begin, end) {
    if (begin == end) return;
    if (a[end - 1] < a[begin]) swap(a, begin, end - 1);

    var lo = begin + 1, hi = end - 2;
    while (lo <= hi) {
        while (a[hi] > a[begin]) --hi;
        while (a[lo] < a[begin]) ++lo;
        if (lo <= hi) swap(a, ++lo, --hi);
    }
    --lo;

    swap(a, begin, lo);
    _qsort3(a, begin, lo);
    _qsort3(a, lo + 1, end);
}

function __qsort4(a, cmp) {
    var newa = a.slice();
    _qsort4(newa, 0, newa.length, cmp);
    return newa;
}
function _qsort4(a, begin, end, cmp) {
    if (begin == end) return;
    if (cmp(a[end - 1], a[begin])) swap(a, begin, end - 1);

    var lo = begin + 1, hi = end - 2;
    while (lo <= hi) {
        while (cmp(a[begin], a[hi])) --hi;
        while (cmp(a[lo], a[begin])) ++lo;
        if (lo <= hi) swap(a, ++lo, --hi);
    }
    --lo;

    swap(a, begin, lo);
    _qsort4(a, begin, lo, cmp);
    _qsort4(a, lo + 1, end, cmp);
}
var qsort4 = function(a){ return __qsort4(a, function(a,b){return a < b;});}
qsort4([]);

var qsort5 = function() {
    var template = 
    '(function _qsort4(a, begin, end) {'+
        'if (begin == end) return;'+
        'if (cmp(a[end - 1], a[begin])) swap(a, begin, end - 1);'+

        'var lo = begin + 1, hi = end - 2;'+
        'while (lo <= hi) {'+
            'while (cmp(a[begin], a[hi])) --hi;'+
            'while (cmp(a[lo], a[begin])) ++lo;'+
            'if (lo <= hi) swap(a, ++lo, --hi);'+
        '}'+
        '--lo;'+

        'swap(a, begin, lo);'+
        '_qsort4(a, begin, lo);'+
        '_qsort4(a, lo + 1, end);'+
    '})';
    
    var str = template.replace(/cmp\(([^,]+),([^\)]+)\)/g, '$1 < $2');
    var f = eval(str);
    return function(a){ 
        var newa = a.slice();
        f(newa, 0, newa.length);
        return newa;
    };
}();
qsort5([]);
//------------------------------

//(function(f) {
//    console.log(f([]));
//    console.log(f([3]));
//    console.log(f([3, 1]));
//    console.log(f([5, 2, 4, 1, 3]));
//}(qsort5))

//------------------------------
var TIMES = 3
var LEN = 1024 * 1024
var arrays = [];
for (var i = 0; i < TIMES; ++i) {
    arrays.push([]);
    for (var j = 0; j < LEN; ++j) {
        arrays[arrays.length - 1].push(Math.floor(Math.random() * LEN));
    }
}

var i = 0;
timeit(TIMES, function() {
    qsort3(arrays[i++ % arrays.length]);
})
