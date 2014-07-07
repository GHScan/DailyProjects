'use strict';

var utils = require('./utils.js');
//------------------------------
function qsort_classic(data) {
    if (data.length <= 1) return data;

    var smaller = data.filter(function(v, i) {
        return i > 0 && v <= data[0];
    });
    var bigger = data.filter(function(v, i) {
        return i > 0 && v > data[0];
    });

    return qsort_classic(smaller).concat(data[0], qsort_classic(bigger));
}
//------------------------------
function qsort_classic_3way(data) {
    if (data.length <= 1) return data;

    var smaller = data.filter(function(v) {
        return v < data[0];
    });
    var equals = data.filter(function(v) {
        return v == data[0];
    });
    var bigger = data.filter(function(v) {
        return v > data[0];
    });

    return qsort_classic(smaller).concat(equals, qsort_classic(bigger));
}

//------------------------------
function _qsort_inplace_2way(data, begin, end) {
    if (end - begin <= 1) return;

    var m = begin + 1;
    for (var i = begin + 1; i < end; ++i) {
        if (data[i] <= data[begin])  {
            var t = data[m]; data[m] = data[i]; data[i] = t;
            ++m;
        }
    }

    --m;
    var t = data[m]; data[m] = data[begin]; data[begin] = t;

    _qsort_inplace_2way(data, begin, m);
    _qsort_inplace_2way(data, m + 1, end);
}

function qsort_inplace_2way(data) {
    _qsort_inplace_2way(data, 0, data.length);
    return data;
}

//------------------------------
function _qsort_inplace_backward_2way(data, begin, end) {
    if (end - begin <= 1) return;

    var m = end - 1;
    for (var i = m; i > begin; --i) {
        if (data[i] > data[begin]) {
            var t = data[i]; data[i] = data[m]; data[m] = t;
            --m;
        }
    }

    var t = data[m]; data[m] = data[begin]; data[begin] = t;

    _qsort_inplace_backward_2way(data, begin, m);
    _qsort_inplace_backward_2way(data, m + 1, end);
}

function qsort_inplace_backward_2way(data) {
    _qsort_inplace_backward_2way(data, 0, data.length);
    return data;
}
//------------------------------
function _qsort_inplace_3way(data, begin, end) {
    if (end - begin <= 1) return;

    var lo = begin + 1;
    var hi = end - 1;

    for (var i = lo; i <= hi;) {
        if (data[i] < data[begin]) {
            var t  = data[lo]; data[lo] = data[i]; data[i] = t;

            ++lo;
            ++i;
        } else if (data[i] > data[begin]) {
            var t = data[hi]; data[hi] = data[i]; data[i] = t;

            --hi;
        } else {
            ++i;
        }
    }

    --lo;
    var t = data[begin]; data[begin] = data[lo]; data[lo] = t;

    _qsort_inplace_3way(data, begin, lo);
    _qsort_inplace_3way(data, hi + 1, end);
}

function qsort_inplace_3way(data) {
    _qsort_inplace_3way(data, 0, data.length);
    return data;
}
//------------------------------
function _qsort_optimal(data, begin, end) {
    if (end - begin <= 2) { 
        if (end - begin == 2 && data[begin] > data[begin + 1]) {
            var t = data[begin]; data[begin] = data[begin + 1]; data[begin + 1] = t;
        } 
        return;
    }

    if (data[begin] > data[end - 1]) {
        var t = data[begin]; data[begin] = data[end - 1]; data[end - 1] = t;
    }
 
    var lo = begin + 1;
    var hi = end - 2;

    while (lo <= hi) {
        while (data[lo] < data[begin]) ++lo;
        while (data[hi] > data[begin]) --hi;
        if (lo <= hi) {
            var t = data[lo]; data[lo] = data[hi]; data[hi] = t;
            ++lo;
            --hi;
        }
    }

    --lo;
    var t = data[begin]; data[begin] = data[lo]; data[lo] = t;

    _qsort_optimal(data, begin, lo);
    _qsort_optimal(data, hi + 1, end);
}

function qsort_optimal(data) {
    _qsort_optimal(data, 0, data.length);
    return data;
}
//------------------------------
function shellSort(data) {
    var step = 1;
    while (step < data.length) step *= 3;

    var len = data.length;
    for (step = Math.floor(step / 3); step >= 1; step = Math.floor(step / 3)) {
        for (var i = step; i < len; ++i) {
            for (var j = i; j - step >= 0 && data[j - step] > data[j]; j -= step) {
                var t = data[j]; data[j] = data[j - step]; data[j - step] = t;
            }
        }
    }

    return data;
}
//------------------------------
function mergeSort_classic(data) {
    if (data.length <= 1) return data;

    var mid = data.length >> 1;

    var data0 = mergeSort_classic(data.slice(0, mid));
    var data1 = mergeSort_classic(data.slice(mid));

    var result = [];
    var i0 = 0, i1 = 0;

    while (i0 < data0.length && i1 < data1.length) {
        if (data0[i0] < data1[i1]) {
            result.push(data0[i0++]);
        } else {
            result.push(data1[i1++]);
        }
    }

    while (i0 < data0.length) result.push(data0[i0++]);
    while (i1 < data1.length) result.push(data1[i1++]);

    return result;
}
//------------------------------
function _merge(rdata, rbegin, data0, begin0, end0, data1, begin1, end1) {
    while (begin0 < end0 && begin1 < end1) {
        if (data0[begin0] < data1[begin1]) {
            rdata[rbegin++] = data0[begin0++];
        } else {
            rdata[rbegin++] = data1[begin1++];
        }
    }

    while (begin0 < end0) rdata[rbegin++] = data0[begin0++];
    while (begin1 < end1) rdata[rbegin++] = data1[begin1++];
}

function _mergeSort_inplace(data, begin, end, temp, tbegin) {
    if (end - begin <= 1) return;

    var len = end - begin;
    var half = len >> 1;
    var mid = begin + half;
    var tmid = tbegin + half;
    var tend = tbegin + len;

    _mergeSort_to(temp, tbegin, data, begin, mid);
    _mergeSort_to(temp, tmid, data, mid, end);
    _merge(data, begin, temp, tbegin, tmid, temp, tmid, tend);
}

function _mergeSort_to(rdata, rbegin, data, begin, end) {
    if (end - begin <= 1) {
        if (end - begin == 1) {
            rdata[rbegin] = data[begin];
        }
        return;
    }

    var mid = ((end - begin) >> 1) + begin;

    _mergeSort_inplace(data, begin, mid, rdata, rbegin);
    _mergeSort_inplace(data, mid, end, rdata, rbegin);
    _merge(rdata, rbegin, data, begin, mid, data, mid, end);
}

function mergeSort_optimal(data) {
    var temp = new Array(data.length);
    _mergeSort_inplace(data, 0, data.length, temp, 0);
    return data;
}

//------------------------------
function qsort_builtin(data) {
    data.sort(function(a, b){ return a - b;});
    return data;
}

//------------------------------
function correctnessTest(funcList) {
    var datas = [0, 1, 3, 4, 5, 7, 8, 11, 15, 16, 17, 25, 31, 32, 64, 127, 257].map(function(len) {
        var a = new Array(len);
        for (var i = 0; i < len; ++i) a[i] = utils.randomInt(0, len);
        return a;
    });

    for (var fi = 0; fi < funcList.length; ++fi) {
        var func = funcList[fi];
        for (var di = 0; di < datas.length; ++di) {
            var data = datas[di].concat();
            var data2 = data.concat();

            data = func.func(data);
            data2.sort(function(a, b){ return a - b;});

            console.assert(utils.equals(data, data2), 'sort failed: ' + func.func.name + ' => ' + datas[di].concat().slice(0, 10) + ' ...');
        }
    }
}
//------------------------------
function benchmark(funcList) {
    var datas = [
        {name: 'ordered', data:(function() {
            var a = new Array(2 * 1024);
            for (var i = 0; i < a.length; ++i) a[i] = i;
            return a;
        })()},
        {name: 'half-repeat', data:(function() {
            var a = new Array(2 * 1024);
            for (var i = 0; i < a.length; ++i) a[i] = i & 1 == 1 ? 1024 : i;
            return a;
        })()},
        {name: 'full-repeat', data:(function() {
            var a = new Array(2 * 1024);
            for (var i = 0; i < a.length; ++i) a[i] = 1024;
            return a;
        })()},
        {name: 'random', data:(function() {
            var a = new Array(2 * 1024);
            for (var i = 0; i < a.length; ++i) a[i] = utils.randomInt(0, a.length);
            return a;
        })()},
        {name: 'random', data:(function() {
            var a = new Array(64 * 1024);
            for (var i = 0; i < a.length; ++i) a[i] = utils.randomInt(0, a.length);
            return a;
        })()},
        {name: 'random', data:(function() {
            var a = new Array(256 * 1024);
            for (var i = 0; i < a.length; ++i) a[i] = utils.randomInt(0, a.length);
            return a;
        })()},
        {name: 'random', data:(function() {
            var a = new Array(1024 * 1024);
            for (var i = 0; i < a.length; ++i) a[i] = utils.randomInt(0, a.length);
            return a;
        })()},
    ];

    for (var di = 0; di < datas.length; ++di) {
        var data = datas[di];

        console.log('>> ' + data.name + '[' + data.data.length/1024 + 'k]' + ':');
        for (var fi = 0; fi < funcList.length; ++fi) {
            var func = funcList[fi];
            if (func.maxLength < data.data.length) continue;

            var times = [];
            for (var i = 0; i < 3; ++i) {
                var tdata = data.data.concat();
                times.push(utils.measureTime(function() {
                    func.func(tdata);
                }));
            }
            console.log(func.func.name + " : " + Math.min.apply(Math, times) / 1000 + 's');
        }
    }
}

//------------------------------
var gFuncList = [
    { maxLength:128*1024, func:qsort_classic, },
    { maxLength:128*1024, func:qsort_classic_3way, },
    { maxLength:1*1024*1024, func:qsort_inplace_2way, },
    { maxLength:1*1024*1024, func:qsort_inplace_backward_2way, },
    { maxLength:1*1024*1024, func:qsort_inplace_3way, },
    { maxLength:1*1024*1024, func:qsort_optimal, },
    { maxLength:256*1024, func:shellSort, },
    { maxLength:256*1024, func:mergeSort_classic, },
    { maxLength:256*1024, func:mergeSort_optimal, },
    { maxLength:1*1024*1024, func:qsort_builtin, },
];

correctnessTest(gFuncList);
benchmark(gFuncList);
