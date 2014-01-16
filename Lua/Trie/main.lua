#! /usr/bin/env lua

local trie = {}
local RES_KEY = {}
function insert(s)
    for i = 1, #s do
        _insert(trie, string.sub(s, i), 0, i, s)
    end
end
function _insert(tree, s, d, pos, src)
    if d > 0 then
        local res = tree[RES_KEY] or {}
        tree[RES_KEY] = res
        res[src] = true
        --res[#res + 1] = {pos, pos + d - 1, src}
    end
    if #s > 0 then
        local c = string.sub(s, 1, 1)
        local bran = tree[c] or {}
        tree[c] = bran
        _insert(bran, string.sub(s, 2), d + 1, pos, src)
    end
end
function get(s)
    local tree = trie
    for i = 1, #s do
        tree = tree[string.sub(s, i, i)]
        if not tree then return end
    end
    return tree[RES_KEY]
end
function clear()
    trie = {}
end

function printRes(res)
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
