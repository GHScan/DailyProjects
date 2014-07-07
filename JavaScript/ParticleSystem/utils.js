
Math.randomChoice = function(a) {
    return a[this.floor(this.random() * a.length)];
};

Math.randomNumber = function(begin, end) {
    return Math.random() * (end - begin) + begin;
};
