
a = {5, 3, 4, 2, 1}
table.sort(a, function(a, b) return a > b end)
table.foreach(a, print)

a.name = 'fjdksl'
function a:printName()
    print(self.name)
end

a.printName(a)
a:printName()
