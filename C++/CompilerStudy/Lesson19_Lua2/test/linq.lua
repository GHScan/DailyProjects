
function range(n)
    local i = 0
    return function()
        local r = i 
        i = i + 1
        if r >= n then return
        else return r end
    end
end

function map(iter, func)
    return function()
        local k = iter()
        if k then return func(k) 
        else return k end
    end
end

function filter(iter, func)
    return function()
        while true do
            local k = iter()
            if not k then return end
            if func(k) then return k end
        end
    end
end

function taken(iter, n)
    return function()
        if n == 0 then return end
        local k = iter()
        if not k then return end
        n = n - 1
        return k
    end
end

function tolist(iter)
    local a = {}
    while true do
        local v = iter()
        if not v then break end
        table.insert(a, v)
    end
    return a
end

function printlist(a)
    table.foreach(a, function(_, v) io.write(v, ',') end)
    print()
end

printlist(tolist(
    taken(
    map(
    filter(
    range(10000000), function(k)
        return k % 2 == 1
    end), function(k)
        return k * k
    end), 10)
))
