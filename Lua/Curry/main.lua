function curry(...)
    local n, f, args = select(1, ...), select(2, ...), {select(3, ...)}
    local curried
    curried = function(...)
        local newArgs = {...}
        for _, arg in ipairs(newArgs) do args[#args + 1] = arg end
        if #args >= n then
            return f(unpack(args))
        else
            return curried
        end
    end
    return curried
end


--
local function sum4(a, b, c, d)
    return a + b + c + d
end
print(sum4(1, 2, 3, 4))
print(curry(4, sum4)(1)(2)(3)(4))
print(curry(4, sum4)(1, 2)(3)(4))
print(curry(4, sum4)(1, 2)(3, 4))
print(curry(4, sum4)(1, 2, 3, 4))
print(curry(4, sum4, 1, 2)(3, 4))
