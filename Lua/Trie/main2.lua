#! /usr/bin/env lua

local trie = {}
local string_sub = string.sub
local function _insert(tree, src, start)
    for i = start, #src + 1 do
        if i > start then
            local res = tree.res or {}
            tree.res = res
            res[src] = true
        end
        if i > #src then break end
        local c = string_sub(src, i, i)
        local bran = tree[c] or {}
        tree[c] = bran
        tree = bran
    end
end
local function insert(s)
    for i = 1, #s do
        _insert(trie, s, i, 0)
    end
end
local function get(s)
    local tree = trie
    for i = 1, #s do
        tree = tree[string_sub(s, i, i)]
        if not tree then return end
    end
    return tree.res
end
local function clear()
    trie = {}
end

local function printRes(res)
    if not res then return end
    local t = {}
    for k, _ in pairs(res) do t[#t + 1] = k end
    print(table.concat(t, ','))
end

-- test1
--local strlist = {
--    'abc',
--    'deffdjk',
--    'ddbba',
--    'abbaa',
--    'df',
--    'fjdskafjk',
--    'iiee',
--    'ppabnba',
--    'fdsii',
--}
--for _, v in ipairs(strlist) do
--    insert(v)
--end
--
--while true do
--    local line = io.read('*l')
--    if #line == 0 then break end
--    printRes(get(line))
--end

-- test2
print('before gc : ', collectgarbage('collect'))
local n = 0
local start = os.clock()
for w in string.gmatch(io.open('1.txt', 'r'):read('*a'), '%a+') do
    insert(w)
    n = n + 1
end
print(string.format('read and insert %d words, %f s', n, os.clock() - start))
print('after gc : count', collectgarbage('count'))
print('after gc : ', collectgarbage('collect'))
print('after gc : count', collectgarbage('count'))

while true do
    local line = io.read('*l')
    if #line == 0 then break end
    printRes(get(line))
end
