
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

------------------------------
function bst_vcps_traverse(node, k)
    if node then
        return bst_vcps_traverse(node[2], function(left)
            return {node[1], function(v)
                return bst_vcps_traverse(node[3], k)
            end}
        end)
    else
        return k(nil)
    end
end

function bst_iterator(bst)
    local vc = bst_vcps_traverse(bst, function(v) return {} end)
    return function()
        if vc[1] then
            local r = vc[1]
            vc = vc[2](vc[1])
            return r
        end
    end
end

------------------------------
local bst = nil
for i = 1, 10 do
    bst = bst_insert(bst, math.random(30))
end

bst_infix_traverse(bst, print)

for v in bst_iterator(bst) do
    print(v)
end
