
function curry(f)
    local args = {}
    local proxy
    proxy = function(x)
        if x then
            args[#args + 1] = x
            return proxy
        else
            return f(unpack(args))
        end
    end
    return proxy
end
function curryN(f, n)
    local args = {}
    local proxy
    proxy = function(x)
        if n == 0 then
            return f(unpack(args))
        else
            n = n - 1
            args[#args + 1] = x
            return proxy
        end
    end
    return proxy
end

--
local function sum4(a, b, c, d)
    return a + b + c + d
end
print(curry(sum4)(1)(2)(3)(4)())
print(curryN(sum4, 4)(1)(2)(3)(4)())
