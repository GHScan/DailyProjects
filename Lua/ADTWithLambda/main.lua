#!/usr/bin/env lua

-- stack
function _stack_init(init, last) return init end
function _stack_last(init, last) return last end
function stack_push(stack, v)
    return function(getter)
        return getter(stack, v)
    end
end
function stack_top(stack)
    return stack(_stack_last)
end
function stack_pop(stack)
    return stack(_stack_init)
end
function stack_traverse(stack, f)
    if not stack then return end
    f(stack(_stack_last))
    stack_traverse(stack(_stack_init), f)
end
-- list
function _list_head(head, tail) return head end
function _list_tail(head, tail) return tail end
function list_node(head, tail)
    return function(getter)
        return getter(head, tail)
    end
end
function list_push_back(list, v)
    if not list then return list_node(v) end
    return list_node(list(_list_head), list_push_back(list(_list_tail), v))
end
function list_push_front(list, v)
    return list_node(v, list)
end
function list_head(list)
    return list(_list_head)
end
function list_tail()
    return list(_list_tail)
end
function list_traverse(list, f)
    if not list then return end
    f(list(_list_head))
    list_traverse(list(_list_tail), f)
end
-- bst
function _bst_value(v, left, right) return v end
function _bst_left(v, left, right) return left end
function _bst_right(v, left, right) return right end
function bst_node(v, left, right)
    return function(getter)
        return getter(v, left, right)
    end
end
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
function bst_min(bst)
    if not bst(_bst_left) then return bst(_bst_value) end
    return bst_min(bst(_bst_left))
end
function bst_remove(bst, v)
    if not bst then return end
    local nv = bst(_bst_value)
    if v < nv then return bst_node(nv, bst_remove(bst(_bst_left), v), bst(_bst_right))
    elseif v > nv then return bst_node(nv, bst(_bst_left), bst_remove(bst(_bst_right), v)) end

    local nv
    if bst(_bst_right) then nv = bst_min(bst(_bst_right)) end
    if not nv then return bst(_bst_left)
    else return bst_node(nv, bst(_bst_left), bst_remove(bst(_bst_right), nv)) end
end
function bst_inorder_traverse(bst, f)
    if not bst then return end
    bst_inorder_traverse(bst(_bst_left), f)
    f(bst(_bst_value))
    bst_inorder_traverse(bst(_bst_right), f)
end

-- main
local bst
table.foreach({3, 2, 5, 1, 4}, function(_, v) bst = bst_insert(bst, v) end)

--table.foreach({3, 2, 5, 1, 4}, function(_, v) bst = bst_remove(bst, v) end)
bst_inorder_traverse(bst, print)
table.foreach({-1, 3, 5, 7}, function(_, v) print(bst_contain(bst, v)) end)


local stack
bst_inorder_traverse(bst, function(v)
    stack = stack_push(stack, v)
end)
stack_traverse(stack, print)

local list
table.foreach({1, 2, 3, 4}, function(_, v) list = list_push_front(list, v) end)
list_traverse(list, print)
