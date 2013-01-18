-- vim:fileencoding=gbk

module(..., package.seeall)

local g_mt = {}

local function _allocateNode(key, realT, parentNode)
    local node = { __key = key, __realT = realT, __parent = parentNode}
    setmetatable(node, g_mt)
    return node
end

local function _getNodePath(node)
    local t = {}
    while node do
        table.insert(t, rawget(node, '__key'))
        node = rawget(node, '__parent')
    end
    local i, j = 1, #t
    while i < j do
        t[i], t[j] = t[j], t[i]
        i, j = i + 1, j - 1
    end
    return table.concat(t, '.')
end

g_mt.__index = function(t, k)
    local v = t.__realT[k]
    if type(v) == 'table' then
        v = _allocateNode(k, v, t)
        return v
    else
        print(_getNodePath(t) .. string.format('.%s(%s)', tostring(k), tostring(v)))
        return v
    end
end

g_mt.__newindex = function(t, k, v)
    print(_getNodePath(t) .. string.format('.%s = %s', tostring(k), tostring(v)))
    t.__realT[k] = v
end

function hook(t)
    local wrap = _allocateNode('', t, nil)
    return wrap
end

function unhook(t)
    return t.__realT
end
