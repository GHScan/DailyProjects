function arrayAppend(a, b)
    local c = {}
    for _, v in ipairs(a) do c[#c + 1] = v end
    for _, v in ipairs(b) do c[#c + 1] = v end
    return c
end

function curry(n, f, ...)
    local boundArgs = arg
    return function(...)
        local fullArgs = arrayAppend(boundArgs, arg)
        if #fullArgs >= n then
            return f(unpack(fullArgs))
        else
            return curry(n, f, unpack(fullArgs))
        end
    end
end

------------------------------
local function sum4(a, b, c, d)
    return a + b + c + d
end
print(sum4(1, 2, 3, 4))
print(curry(4, sum4)(1)(2)(3)(4))
print(curry(4, sum4)(1, 2)(3)(4))
print(curry(4, sum4)(1, 2)(3, 4))
print(curry(4, sum4)(1, 2, 3, 4))
print(curry(4, sum4, 1, 2)(3, 4))

------------------------------
function map(f, array)
    local newarray = {}
    for _, v in ipairs(array) do
        newarray[#newarray + 1] = f(v)
    end
    return newarray
end
function range(last)
    local array = {}
    for i = 1, last do
        array[#array + 1] = i
    end
    return array
end

function add(a, b) return a + b end
function sub(a, b) return a - b end
function mul(a, b) return a * b end
function div(a, b) return a / b end
table.foreach(map(curry(2, mul, 2), range(10)), print)
