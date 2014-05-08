function timing(name, f)
    local start = os.clock()
    f()
    print(name, os.clock() - start)
end

function memoize(f)
    local cache = {}
    return function(x)
        if cache[x] then
            return cache[x]
        else
            local r = f(x)
            cache[x] = r
            return r
        end
    end
end

-- origin fib
function fib(n)
    if n <= 1 then
        return 1
    else
        return fib(n - 1) + fib(n - 2)
    end
end

assert(89 == fib(10))
timing('origin fib', function() fib(34) end)

-- memoized fib
fib = memoize(fib)

assert(89 == fib(10))
timing('memoized fib', function() fib(34) end)
