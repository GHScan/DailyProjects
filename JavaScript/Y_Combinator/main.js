'use strict'; 

//------------------------------
// Y combinator
function Y(f) {
    return (function(g) {
        return g(g);
    })(function(g) {
        return f(function() { return g(g).apply(null, arguments); });
    });
}

//------------------------------
// via Y-combinator
var f = Y(function(self) {
    return function(n) {
        return n < 2 ? n : self(n - 1) + self(n - 2);
    }
});

// via function expression
var f2 = (function f(n) {
    return n < 2 ? n : f(n - 1) + f(n - 2);
})

// via expand function expression
var f3 = (function () {
    var f = function(n) {
        return n < 2 ? n : f(n - 1) + f(n - 2);
    };
    return f;
})();

console.log(f(30));
console.log(f2(30));
console.log(f3(30));
