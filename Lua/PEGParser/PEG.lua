module(..., package.seeall)

function lazy(f)
    local p
    return function(input, onSuccess, onFail)
        if not p then
            p = f()
        end
        return p(input, onSuccess, onFail)
    end
end

function rawterm(pattern)
    local relPattern = '^' .. pattern
    return function(input, onSuccess, onFail)
        local result = string.match(input, relPattern)
        if result then
            return onSuccess(result, string.sub(input, #result + 1))
        else
            return onFail(string.format('Parse failed: expect %s, found %s', pattern, input))
        end
    end
end

whitespace = rawterm('%s*')

function term(pattern)
    return right(whitespace, left(rawterm(pattern), whitespace))
end

function concat(a, b, f)
    f = f or function(a, b) return {a, b} end
    return function(input, onSuccess, onFail)
        return a(input, function(va, input)
            return b(input, function(vb, input)
                return onSuccess(f(va, vb), input)
            end, onFail)
        end, onFail)
    end
end

function choice(a, b)
    return function(input, onSuccess, onFail)
        return a(input, onSuccess, function(serr)
            return b(input, onSuccess, onFail)
        end)
    end
end

function succ(v)
    return function(input, onSuccess, onFail)
        return onSuccess(v, input)
    end
end

function fail(message)
    return function(input, onSuccess, onFail)
        return onFail(string.format('%s: %s', message, input))
    end
end

function map(a, f)
    return function(input, onSuccess, onFail)
        return a(input, function(result, input)
            return onSuccess(f(result), input)
        end, onFail)
    end
end

function flatMap(a, f)
    return function(input, onSuccess, onFail)
        return a(input, function(result, input)
            return f(result)(input, onSuccess, onFail)
        end, onFail)
    end
end

function opt(a)
    return choice(map(a, function(v) return {v} end), succ({}))
end

function rep(a)
    return choice(flatMap(a, function(head)
        return map(rep(a), function(tail)
            table.insert(tail, 1, head)
            return tail
        end)
    end),
    succ({}))
end

function rep1(a)
    return concat(a, rep(a), function(head, tail)
        table.insert(tail, 1, head)
        return tail
    end)
end

function chain(a, sep, ffirst, fop)
    return concat(a, rep(concat(sep, a, function(a, b) return {a, b} end)), function(v, l)
    local r = ffirst(v)
    for _, p in ipairs(l) do
        r = fop(r, p[1], p[2])
    end
    return r
end)
end

function rep1sep(a, sep)
    return chain(a, sep, 
    function(v) return {v} end,
    function(l, _, v) 
        table.insert(l, v)
        return l
    end, {})
end

function repsep(a, sep)
    return choice(rep1sep(a, sep), succ({}))
end

function left(a, b)
    return concat(a, b, function(a, b) return a end)
end

function right(a, b)
    return concat(a, b, function(a, b) return b end)
end

function concatla(a, b)
    return function(input, onSuccess, onFail)
        return a(input, function(result, input)
            return b(input, function()
                return onSuccess(result, input)
            end, onFail)
        end, onFail)
    end
end

function _not(a, result)
    return function(input, onSuccess, onFail)
        return a(input, function()
            return onFail(string.format('Parse failed: not', input))
        end, function()
        return onSuccess(result, input)
    end)
end
end

function phrase(a)
    return function(input, onSuccess, onFail)
        return a(input, function(result, input)
            if #input > 0 then
                return onFail(string.format('Input is too long: %s', input))
            else
                return onSuccess(result, input)
            end
        end, onFail)
    end
end
