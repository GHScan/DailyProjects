
a = {}
for i = 1, 10 do
    table.insert(a, tostring(i))
end
print(collectgarbage())
a = nil
print(collectgarbage())

parent = {}
child = {}
parent.child = child
child.parent = parent
print(collectgarbage())
parent = nil
child = nil
print(collectgarbage())

a = {a={tostring(1), tostring(2), tostring('3')}, b=tostring(123), c=tostring(456)}
print(collectgarbage())
a.a = nil
a.b = nil
a.c = nil
print(collectgarbage())
