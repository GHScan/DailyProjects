
a = {}
setmetatable(a, {
    __index = function(t, k)
        t[k] = _G[k]
        return t[k]
    end
})

table.foreach(a, print)
a.print(1)
table.foreach(a, print)
a = nil

a = {}
setmetatable(a, {
    __gc = function(t)
        print('gc ', t)
    end
})
b = a
print('release a')
a = nil
print('release b')
b = nil
