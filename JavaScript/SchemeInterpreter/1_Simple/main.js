'use strict';

var fs = require('fs');
//------------------------------
function S_parse(s) {
    s = '(' + s + ')';
    s = s.replace(/\(/g, '[');
    s = s.replace(/\)/g, ']');
    s = s.replace(/([^\s\[\].]+)/g, '"$1"');
    s = s.replace(/"(\d+)"/g, '$1');
    s = s.replace(/\s+/g, ',');
    s = s.replace(/,\]/g, ']');
    return JSON.parse(s);
}
//------------------------------
function Pair(car, cdr) {
    this.car = car;
    this.cdr = cdr;
}

Pair.prototype.toString = function() {
    var pair = this;

    if (pair === Pair.empty) {
        return '()';
    }

    var s = '(' + pair.car.toString();

    while (pair.cdr instanceof Pair && pair.cdr !== Pair.empty) {
        pair = pair.cdr;
        s += ' ' + pair.car.toString();
    }

    if (pair.cdr !== Pair.empty) {
        s += ' . ' + pair.cdr.toString();
    }

    s += ')';

    return s;
}

Pair.empty = new Pair(null, null);

function arrayExp2PairExp(e) {
    if (Array.isArray(e)) {
        var pair = Pair.empty;
        for (var i = e.length - 1; i >= 0; --i) {
            pair = new Pair(arrayExp2PairExp(e[i]), pair);
        }
        return pair;
    } else {
        return e;
    }
}

function pairExp2ArrayExp(e) {
    if (e instanceof Pair) {
        var a = [];
        while (e !== Pair.empty) {
            a.push(pairExp2ArrayExp(e.car));
            e = e.cdr;
        }
        return a;
    } else {
        return e;
    }
}
//------------------------------
function S_eval(env, exp) {
    while (true) {
        switch (true) {
            case typeof exp == 'string':
                return env[exp];
            case !Array.isArray(exp):
                return exp;
            case exp[0] === 'quote':
                return arrayExp2PairExp(exp[1]);
            case exp[0] === 'if':
                if (S_eval(env, exp[1])) {
                    exp = exp[2];
                } else {
                    exp = exp[3];
                }
                break;
            case exp[0] === 'lambda':
                return { env: env, formalArgs: exp[1], bodyExps: exp.slice(2)};
            case exp[0] === 'begin':
                for (var i = 1; i < exp.length - 1; ++i) {
                    S_eval(env, exp[i]);
                }
                exp = exp[exp.length - 1];
                break;
            case exp[0] === 'cond': {
                    for (var i = 1; i < exp.length; ++i) {
                        if (S_eval(env, exp[i][0])) {
                            var j = 1;
                            for (; j < exp[i].length - 1; ++j) {
                                S_eval(env, exp[i][j]);
                            }
                            exp = exp[i][j];
                            break;
                        }
                    }
                }
                break;
            case exp[0] === 'define': 
                if (typeof exp[1] == 'string') {
                    env[exp[1]] = S_eval(env, exp[2]);
                } else {
                    env[exp[1][0]] = {env: env, formalArgs: exp[1].slice(1), bodyExps: exp.slice(2)};
                }
                return undefined;
            case exp[0] === 'set!': 
                console.assert(false);
                return undefined;
            case exp[0] === 'let': {
                    var formalArgs = exp[1].map(function(v) { return v[0]; });
                    var actualArgs = exp[1].map(function(v) { return v[1]; });;
                    var bodyExps = exp.slice(2);
                    exp = [['lambda', formalArgs,].concat(bodyExps)].concat(actualArgs);
                }
                break;
            default: {
                    var p = S_eval(env, exp[0]);
                    var actualArgs = [];
                    for (var i = 1; i < exp.length; ++i) {
                        actualArgs.push(S_eval(env, exp[i]));
                    }

                    if (typeof p == 'function') {
                        return p.apply(null, actualArgs);
                    } else {
                        env = Object.create(p.env);

                        console.assert(p.formalArgs.length == actualArgs.length);
                        for (var i = 0; i < actualArgs.length; ++i) {
                            env[p.formalArgs[i]] = actualArgs[i];
                        }

                        for (var i = 0; i < p.bodyExps.length - 1; ++i) {
                            S_eval(env, p.bodyExps[i]);
                        }

                        exp = p.bodyExps[p.bodyExps.length - 1];
                    }
                }
                break;
        }
    }
}

function S_setupG() {
    var G = {
        'true': true,
        'false': false,
        'else': true,
        'empty': Pair.empty,

        'sqr': function(a){ return a * a; },
        'sqrt': function(a){ return Math.sqrt(a); },
        'identity': function(a){ return a; },
        'not': function(a){ return !a; },
        'empty?': function(a){ return a == Pair.empty; },

        '+': function(a, b){ return a + b; },
        '-': function(a, b){ return a - b; },
        '*': function(a, b){ return a * b; },
        '/': function(a, b){ return a / b; },
        'quotient': function(a, b){ return Math.floor(a / b); },
        'remainder': function(a, b){ return a % b; },

        '=': function(a, b){ return a == b; },
        '<': function(a, b){ return a < b; },
        '<=': function(a, b){ return a <= b; },
        '>': function(a, b){ return a > b; },
        '>=': function(a, b){ return a >= b; },
        'eq?': function(a, b){ return a === b; },
        'equal?': function(a, b){ return JSON.stringify(a) === JSON.stringify(b); },

        'cons': function(a, b){ return new Pair(a, b); },
        'car': function(a){ return a.car; },
        'cdr': function(a){ return a.cdr; },
        'drop': function(a, n){
            while (n-- > 0) a = a.cdr;
            return a;
        },
        'append': function(a, b){
            var values = [];
            for (; a != Pair.empty; a = a.cdr) {
                values.push(a.car);
            }
            for (var i = values.length - 1; i >= 0; --i) {
                b = new Pair(values[i], b);
            }
            return b;
        },
        'length': function(a){
            var len = 0;
            for (; a != Pair.empty; a = a.cdr) ++len;
            return len;
        },

        'pretty-print': function(a){
            console.log(a.toString());
            return undefined;
        },
        'current-inexact-milliseconds': function(){
            return new Date().getTime();
        },
        'random': function(n) {
            return Math.floor(n * Math.random());
        },
        'eval': function(a) {
            return S_eval(G, pairExp2ArrayExp(a));
        },
    };
    return G;
}
//------------------------------
var G = S_setupG();

var exps = S_parse(fs.readFileSync(process.argv[2]).toString('utf-8'));

for (var i in exps) {
    var v = S_eval(G, exps[i]);
    if (typeof v != 'undefined') {
        console.log(v.toString());
    }
}
