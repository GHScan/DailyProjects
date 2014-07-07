"use strict";

Math.randomInt = function (first, limit) {
    return this.floor(this.random() * (limit - first)) + first;
};

Array.prototype.findIf = function(f) {
    for (var i = 0; i < this.length; ++i) {
        if (f(this[i])) return i;
    }
    return -1;
};

//-------------------------------------------------------------------------
Number.prototype.equals =
Boolean.prototype.equals =
String.prototype.equals = function (o) {
    return this.valueOf() === o.valueOf();
};

Object.prototype.equals = function (o) {
    if (this === o) return true;
    if (this.constructor !== o.constructor) return false;

    var n = 0;
    for (var name in this) {
        if (!this.hasOwnProperty(name)) continue;
        if ((typeof o[name] == 'undefined') ||
                (!this[name].equals(o[name]))) return false;
        ++n;
    }

    for (var name in o) {
        if (!o.hasOwnProperty(name)) continue;
        --n
    }

    return n == 0;
};

//-------------------------------------------------------------------------