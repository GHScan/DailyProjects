
var start = clock()
for (var i = 0; i < 100; ++i) {
    for (var j = 0; j < 1000; ++j) {
        for (var k = 0; k < 1000; ++k);
    }
}
println([clock() - start])
