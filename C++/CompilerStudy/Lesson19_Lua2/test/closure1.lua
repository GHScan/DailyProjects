
function make()
    local a = {}
    for i = 1, 10 do
        table.insert(a, function()
            print(i * i)
        end)
    end
    for i = 10, 1, -1 do
        table.insert(a, function()
            print(i * i)
        end)
    end
    return function()
        table.foreach(a, function(_, f) f() end)
    end
end

r = make()()
