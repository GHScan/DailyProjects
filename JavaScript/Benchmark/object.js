'use strict';

var utils = require('./utils.js');
//------------------------------

var N = 1024 * 1024;

function VectorPrototype(x, y, z) {
    this.x = x;
    this.y = y;
    this.z = z;
}
VectorPrototype.prototype.sqrLength = function() {
    return this.x * this.x + this.y * this.y + this.z * this.z;
}

function VectorObject(x, y, z) {
    return {x:x, y:y, z:z};
}

function VectorLambda(x, y, z) {
    return function() {
        return x * x + y * y + z * z;
    }
}


utils.timeIt('make vector-prototype', function() {
    for (var i = 0; i < N; ++i) new VectorPrototype(i, i, i);
});

utils.timeIt('make vector-object', function() {
    for (var i = 0; i < N; ++i) VectorObject(i, i, i);
});

utils.timeIt('make vector-lambda', function() {
    for (var i = 0; i < N; ++i) VectorLambda(i, i, i);
});


utils.timeIt('access vector-prototype outside', function() {
    var obj = new VectorPrototype(i, i, i);
    var sum = 0;
    for (var i = 0; i < N; ++i) sum += obj.x * obj.x + obj.y * obj.y + obj.z * obj.z;
});

utils.timeIt('access vector-object outside', function() {
    var obj = VectorObject(i, i, i);
    var sum = 0;
    for (var i = 0; i < N; ++i) sum += obj.x * obj.x + obj.y * obj.y + obj.z * obj.z;
});

utils.timeIt('access vector-prototype inside', function() {
    var obj = new VectorPrototype(i, i, i);
    var sum = 0;
    for (var i = 0; i < N; ++i) sum += obj.sqrLength();
});

utils.timeIt('access vector-lambda inside', function() {
    var obj = VectorLambda(i, i, i);
    var sum = 0;
    for (var i = 0; i < N; ++i) sum += obj();
});
