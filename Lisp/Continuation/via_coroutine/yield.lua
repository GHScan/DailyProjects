dofile("fp.lua")
------------------------------
function iter(f)
    return coroutine.wrap(f)
end
function yield(v)
    coroutine.yield(v)
end
------------------------------
function range(first, last, step)
    if not step then step = 1 end
    if not last then 
        first, last = 1, first
    end
    return iter(function()
        for i = first, last, step do
            yield(i)
        end
    end)
end
function map(proc, ...)
    local result = {}
    local its = arg
    return iter(function()
        while true do
            for i, it in ipairs(its) do
                result[i] = it()
            end
            if not result[1] then break end
            yield(proc(unpack(result)))
        end
    end)
end
function filter(proc, it)
    return iter(function()
        for v in it do
            if proc(v) then 
                yield(v)
            end
        end
    end)
end
------------------------------
for i in filter(function(v) return v % 2 == 1 end, 
        map(curry(2, add, 5), range(10))) do
    print(i)
end
