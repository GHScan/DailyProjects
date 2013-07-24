--vim:fileencoding=utf-8

-- lambda bst
function bst_node(v, left, right)
    return function(getter)
        return getter(v, left, right)
    end
end
function _bst_value(v, left, right) return v end
function _bst_left(v, left, right) return left end
function _bst_right(v, left, right) return right end
function bst_insert(bst, v)
    if not bst then return bst_node(v) end
    local nv = bst(_bst_value)
    if v == nv then return bst
    elseif v < nv then return bst_node(nv, bst_insert(bst(_bst_left), v), bst(_bst_right))
    else return bst_node(nv, bst(_bst_left), bst_insert(bst(_bst_right), v)) end
end
function bst_contain(bst, v)
    if not bst then return false end
    local nv = bst(_bst_value)
    if v == nv then return true
    elseif v < nv then return bst_contain(bst(_bst_left), v)
    else return bst_contain(bst(_bst_right), v) end
end
function bst_traverse(bst, f)
    if not bst then return end
    bst_traverse(bst(_bst_left), f)
    f(bst(_bst_value))
    bst_traverse(bst(_bst_right), f)
end
-- table bst
function tbst_insert(bst, v)
    if not bst then return {v} end
    if v == bst[1] then 
    elseif v < bst[1] then bst[2] = tbst_insert(bst[2], v) 
    else bst[3] = tbst_insert(bst[3], v) end
    return bst
end
function tbst_traverse(bst, f)
    if not bst then return end
    tbst_traverse(bst[2], f)
    f(bst[1])
    tbst_traverse(bst[3], f)
end
function tbst_contain(bst, v)
    if not bst then return false end
    if v == bst[1] then return true
    elseif v < bst[1] then return tbst_contain(bst[2], v)
    else return tbst_contain(bst[3], v) end
end
-- main
function timing(insert_func, contain_func)
    local values = {}
    for i = 1, 10000 do
        table.insert(values, math.random())
    end
    local start = os.clock()
    local bst
    for _, v in ipairs(values) do
        bst = insert_func(bst, v)
    end
    for i = 1, 10 do
        for _, v in ipairs(values) do
            contain_func(bst, v)
        end
    end
    print(os.clock() - start, 'sec')
end

math.randomseed(os.time())
timing(bst_insert, bst_contain)
timing(tbst_insert, tbst_contain)
