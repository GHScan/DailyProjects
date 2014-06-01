
function bst_insert(node, v)
    if not node then 
        node = {v, nil, nil}
    elseif v < node[1] then
        node[2] = bst_insert(node[2], v)
    elseif v > node[1] then
        node[3] = bst_insert(node[3], v)
    end
    return node
end

------------------------------
function bst_infix_traverse(node, f)
    if node then
        bst_infix_traverse(node[2], f)
        f(node[1])
        bst_infix_traverse(node[3], f)
    end
end

function bst_linear_iterator(bst)
    local a = {}
    bst_infix_traverse(bst, function(v) a[#a + 1] = v end)
    local i = 1
    return function()
        local r = a[i]
        i = i + 1
        return r
    end
end
------------------------------
function bst_vcps_traverse(node, k)
    if node then
        return bst_vcps_traverse(node[2], function(left)
            return node[1], function(v)
                return bst_vcps_traverse(node[3], k)
            end
        end)
    else
        return k(nil)
    end
end

function bst_vcps_iterator(bst)
    local v, k = bst_vcps_traverse(bst, function(v) return v, nil end)
    return function()
        if k then
            local r = v
            v, k = k(v)
            return r
        end
    end
end
------------------------------
function bst_coroutine_traverse(node)
    if node then
        bst_coroutine_traverse(node[2])
        coroutine.yield(node[1])
        bst_coroutine_traverse(node[3])
    end
end

function bst_coroutine_iterator(bst)
    return coroutine.wrap(function()
        bst_coroutine_traverse(bst)
    end)
end

------------------------------
-- testing
local bst = nil
for i = 1, 10 do
    bst = bst_insert(bst, math.random(30))
end

bst_infix_traverse(bst, print)

for v in bst_linear_iterator(bst) do
    print(v)
end
for v in bst_vcps_iterator(bst) do
    print(v)
end
for v in bst_coroutine_iterator(bst) do
    print(v)
end

------------------------------
-- benchmark
function timing(name, loop, f)
    local start = os.clock()
    for i = 1, loop do f() end
    print(name, os.clock() - start)
end

bst = nil
for i = 1, 10000 do
    bst = bst_insert(bst, math.random(20000))
end

print('benchmark:')
timing(string.format('\t%20s', 'linear'), 100, function()
    for _ in bst_linear_iterator(bst) do end
end)
timing(string.format('\t%20s', 'coroutine'), 100, function()
    for _ in bst_coroutine_iterator(bst) do end
end)
timing(string.format('\t%20s', 'vcps'), 100, function()
    for _ in bst_vcps_iterator(bst) do end
end)
