
a = [1, 2, 3]
a = ['123', tostring(3)]
a = 'a' + 4
println(a)
println(collectgarbage())
for (var i = 0; i < 10; ++i) {
    a = [tostring(i)]
}
println(collectgarbage())
