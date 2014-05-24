function arrayCopy(a)
    local r = {}
    for i, v in ipairs(a) do
        r[i] =v
    end
    return r
end
function arrayConcat(a, b)
    for _, v in ipairs(b) do
        a[#a + 1] = v
    end
    return a
end
------------------------------
function curry(n, f, ...)
    local freeArgs = arg
    return function(...)
        local allArgs = arrayConcat(arrayCopy(freeArgs), arg)
        if #allArgs >= n then 
            return f(unpack(allArgs))
        else
            return curry(n, f, unpack(allArgs))
        end
    end
end
function folda(proc, init, ...)
    for _, v in ipairs(arg) do
        init = proc(init, v)
    end
    return init
end
function mapa(proc, ...)
    for _, v in ipairs(arg) do
        arg[i] = proc(v)
    end
    return unpack(arg)
end
function filtera(proc, ...)
    local i = 1
    for _, v in ipairs(arg) do
        if proc(v) then
            arg[i] = v
            i = i + 1
        end
    end
    arg[i] = nil
    return unpack(arg)
end
------------------------------
function add(...) 
    return folda(function(a, b) return a + b end, 0, ...)
end
function sub(...) 
    return folda(function(a, b) return a - b end, select(1, ...), select(2, ...))
end
function mul(...) 
    return folda(function(a, b) return a * b end, 1, ...)
end
function div(...) 
    return folda(function(a, b) return a / b end, select(1, ...), select(2, ...))
end
function mod(...) 
    return folda(function(a, b) return a % b end, select(1, ...), select(2, ...))
end
function pow(...) 
    return folda(function(a, b) return math.pow(a, b) end, select(1, ...), select(2, ...))
end
