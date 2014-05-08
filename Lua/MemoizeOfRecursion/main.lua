function timing(name, f)
    local start = os.clock()
    f()
    print(name, os.clock() - start)
end

function memoize(f)
    local cache = {}
    return function(x)
        if not cache[x] then 
            cache[x] = f(x)
        end
        return cache[x]
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
