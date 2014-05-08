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

-- test1: fibonacci
-- origin 
function fib(n)
    if n <= 1 then
        return 1
    else
        return fib(n - 1) + fib(n - 2)
    end
end

assert(89 == fib(10))
timing('origin fib', function() fib(34) end)

-- memoized
fib = memoize(fib)

assert(89 == fib(10))
timing('memoized fib', function() fib(34) end)

-- test2: find shortestPalindrome
-- origin 
function shortestPalindrome(s)
    if #s <= 1 then return s end
    local first, last = string.sub(s, 1, 1), string.sub(s, #s, #s)
    if first == last then return first .. shortestPalindrome(string.sub(s, 2, -2)) .. last end
    local s1 = first .. shortestPalindrome(string.sub(s, 2)) .. first
    local s2 = last .. shortestPalindrome(string.sub(s, 1, -2)) .. last
    if #s1 < #s2 then return s1
    else return s2 end
end

assert('fessef' == shortestPalindrome('fesse'))
assert('dcbabcd' == shortestPalindrome('cbabd'))
timing('origin shortestPalindrome', function() shortestPalindrome('aababbadfdabadssaffababbafss') end)

-- memoized 
shortestPalindrome = memoize(shortestPalindrome)
assert('fessef' == shortestPalindrome('fesse'))
assert('dcbabcd' == shortestPalindrome('cbabd'))
timing('memoized shortestPalindrome', function() shortestPalindrome('aababbadfdabadssaffababbafss') end)
