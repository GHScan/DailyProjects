'use strict';

exports.randomInt = function(first, limit) {
    return first + Math.floor(Math.random() * (limit - first));
};

exports.timeIt = function(label, f) {
    console.time(label);
    f();
    console.timeEnd(label);
}

exports.measureTime = function(f) {
    var start = new Date();
    f();
    return new Date() - start;
}

exports.equals = function(a, b) {
    if (a === b) return true;
    if (typeof a != 'object' || typeof b != 'object') return false;
    if (a == null || b == null) return false;
    if (a.constructor != b.constructor) return false;

    var n = 0;
    for (var name in a) {
        if (!a.hasOwnProperty(name)) continue;
        if (!this.equals(a[name], b[name])) return false;
        ++n;
    }

    for (var name in b) {
        if (!b.hasOwnProperty(name)) continue;
        --n;
    }

    return n == 0;
}
