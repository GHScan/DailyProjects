'use strict';

var utils = require('./utils.js');
//------------------------------
function Node(value, left, right) {
    this.value = value;
    this.left = left;
    this.right = right;
}

function bstInsert(node, value) {
    switch (true) {
        case node == null:
            node = new Node(value, null, null);
            break;
        case value == node.value:
            break;
        case value < node.value:
            node.left = bstInsert(node.left, value);
            break;
        default:
            node.right = bstInsert(node.right, value);
            break;
    }
    return node;
}

function bstInfixTraverse(node, f) {
    if (node != null) {
        bstInfixTraverse(node.left, f);
        f(node.value);
        bstInfixTraverse(node.right, f);
    }
}

function bstVCPSTraverse(node, k) {
    if (node == null) return k();
    else return bstVCPSTraverse(node.left, function() {
        return {value:node.value, k:function() {
            return bstVCPSTraverse(node.right, k);
        }};
    });
}

function bstArrayIterator(node) {
    var a = [];
    bstInfixTraverse(node, function(v) {
        a.push(v);
    });

    var i = 0;
    return function() {
        if (i < a.length) return a[i++];
        else return undefined;
    };
}

function bstVCPSIterator(node) {
    var pair = bstVCPSTraverse(node, function(){ return undefined; });

    return function() {
        if (typeof pair == 'undefined') {
            return pair;
        } else {
            var v = pair.value;
            pair = pair.k();
            return v;
        }
    };
}

//------------------------------
// testing
var bst = null;
for (var i = 0; i < 10; ++i) {
    bst = bstInsert(bst, utils.randomInt(0, 30));
}

console.log('>> testing');

console.log('infix traverse:');
bstInfixTraverse(bst, function(v){ console.log(v); });

console.log('array iterator:');
var iter = bstArrayIterator(bst);
for (var v = iter(); v != undefined; v = iter()) {
    console.log(v);
}

console.log('vcps iterator:');
var iter = bstVCPSIterator(bst);
for (var v = iter(); v != undefined; v = iter()) {
    console.log(v);
}
//------------------------------
// benchmark

bst = null; 
for (var i = 0; i < 10 * 1024; ++i) {
    bst = bstInsert(bst, utils.randomInt(0, 1024 * 1024));
}

console.log('>> bencmark');

utils.timeIt("full iterate array iterator", function() {
    for (var i = 0; i < 100; ++i) {
        var iter = bstArrayIterator(bst);
        for (var v = iter(); v != undefined; v = iter());
    }
});

utils.timeIt("full iterate VCPS iterator", function() {
    for (var i = 0; i < 100; ++i) {
        var iter = bstVCPSIterator(bst);
        for (var v = iter(); v != undefined; v = iter());
    }
});

utils.timeIt("iterate top10 array iterator", function() {
    for (var i = 0; i < 1000; ++i) {
        var iter = bstArrayIterator(bst);
        for (var j = 0; j < 10; ++j) var v = iter();
    }
});

utils.timeIt("iterate top10 VCPS iterator", function() {
    for (var i = 0; i < 1000; ++i) {
        var iter = bstVCPSIterator(bst);
        for (var j = 0; j < 10; ++j) var v = iter();
    }
});
