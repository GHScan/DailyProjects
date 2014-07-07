'use strict';

Math.randomChoice = function(a) {
    return a[this.floor(this.random() * a.length)];
}

Array.prototype.all = function(f) {
    for (var i = this.length - 1; i >= 0; --i) {
        if (!f(this[i])) return false;
    }
    return true;
}
