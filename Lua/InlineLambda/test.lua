-- vim:fileencoding=gbk

function timeit(times, f)
    if times > 1 then f() end
    local start = os.clock()
    for i  = 1, times do f() end
    print((os.clock() - start) / times)
end

function clear(a)
    for k, v in pairs(a) do a[k] = v end
end

function printArray(a)
    for _, v in ipairs(a) do io.write(v, ',') end
    print()
end
------------------------------
local function filter(newa, a, f)
    local j = 1
    for i = 1, #a do
        if f(a[i]) then 
            newa[j] = a[i]
            j = j + 1
        end
    end
    return newa
end
local function map(newa, a, f)
    local j = 1
    for i = 1, #a do
        newa[i] = f(a[i])
    end
    return newa
end
local function reduce(a, f, init)
    local v = init
    for i = 1, #a do
        v = f(v, a[i])
    end
    return v
end
------------------------------ 
local filterCache = {}
local function filter2(newa, a, fs)
    local f = filterCache[fs]
    if not f then
        local s = string.gsub([[
            return function(newa, a)
                local j = 1
                for i = 1, #a do
                    if @(a[i]) then 
                        newa[j] = a[i]
                        j = j + 1
                    end
                end
            end
        ]], '@%(([^%)]+)%)', fs)
        f = assert(loadstring(s))()
        filterCache[fs] = f
    end
    f(newa, a)
    return newa
end
local mapCache = {}
local function map2(newa, a, fs)
    local f = mapCache[fs]
    if not f then
        local s = string.gsub([[
            return function(newa, a)
                local j = 1
                for i = 1, #a do
                    newa[i] = @(a[i])
                end
            end
        ]], '@%(([^%)]+)%)', fs)
        f = assert(loadstring(s))()
        mapCache[fs] = f
    end
    f(newa, a)
    return newa
end
local reduceCache = {}
local function reduce2(a, fs, init)
    local f = reduceCache[fs]
    if not f then
        local s = string.gsub([[
            return function(a, init)
                local v = init
                for i = 1, #a do
                    v = @(v, a[i])
                end
                return v
            end
        ]], '@%(([^,]+),([^%)]+)%)', fs)
        f = assert(loadstring(s))()
        reduceCache[fs] = f
    end
    return f(a, init)
end
------------------------------
local a = {}
for i = 1, 1024*1024 do a[i] = i end
local newa = {}
for k, v in ipairs(a) do newa[k] = v end

timeit(10, function()
    filter(newa, a, function(v) return v % 2 == 0 end)
end)
timeit(10, function()
    map(newa, a, function(v) return v + v end)
end)
timeit(10, function()
    reduce(a, function(a, b) return a + b end, 0)
end)

timeit(10, function()
    filter2(newa, a, '%1 %% 2==0')
end)
timeit(10, function()
    map2(newa, a, '%1 + %1')
end)
timeit(10, function()
    reduce2(a, '%1 + %2', 0)
end)
