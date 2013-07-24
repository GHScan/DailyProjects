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
-- bsp
function _bsp_value(v, left, right) return v end
function _bsp_left(v, left, right) return left end
function _bsp_right(v, left, right) return right end
function bsp_node(v, left, right)
    return function(getter)
        return getter(v, left, right)
    end
end
function bsp_insert(bsp, v)
    if not bsp then return bsp_node(v) end
    local nv = bsp(_bsp_value)
    if v == nv then return bsp 
    elseif v < nv then return bsp_node(nv, bsp_insert(bsp(_bsp_left), v), bsp(_bsp_right))
    else return bsp_node(nv, bsp(_bsp_left), bsp_insert(bsp(_bsp_right), v)) end
end
function bsp_contain(bsp, v)
    if not bsp then return false end
    local nv = bsp(_bsp_value)
    if v == nv then return true
    elseif v < nv then return bsp_contain(bsp(_bsp_left), v)
    else return bsp_contain(bsp(_bsp_right), v) end
end
function bsp_min(bsp)
    if not bsp(_bsp_left) then return bsp(_bsp_value) end
    return bsp_min(bsp(_bsp_left))
end
function bsp_remove(bsp, v)
    if not bsp then return end
    local nv = bsp(_bsp_value)
    if v < nv then return bsp_node(nv, bsp_remove(bsp(_bsp_left), v), bsp(_bsp_right))
    elseif v > nv then return bsp_node(nv, bsp(_bsp_left), bsp_remove(bsp(_bsp_right), v)) end

    local nv
    if bsp(_bsp_right) then nv = bsp_min(bsp(_bsp_right)) end
    if not nv then return bsp(_bsp_left)
    else return bsp_node(nv, bsp(_bsp_left), bsp_remove(bsp(_bsp_right), nv)) end
end
function bsp_inorder_traverse(bsp, f)
    if not bsp then return end
    bsp_inorder_traverse(bsp(_bsp_left), f)
    f(bsp(_bsp_value))
    bsp_inorder_traverse(bsp(_bsp_right), f)
end

-- main
local bsp
table.foreach({3, 2, 5, 1, 4}, function(_, v) bsp = bsp_insert(bsp, v) end)

--table.foreach({3, 2, 5, 1, 4}, function(_, v) bsp = bsp_remove(bsp, v) end)
bsp_inorder_traverse(bsp, print)
table.foreach({-1, 3, 5, 7}, function(_, v) print(bsp_contain(bsp, v)) end)


local stack
bsp_inorder_traverse(bsp, function(v)
    stack = stack_push(stack, v)
end)
stack_traverse(stack, print)

local list
table.foreach({1, 2, 3, 4}, function(_, v) list = list_push_front(list, v) end)
list_traverse(list, print)
