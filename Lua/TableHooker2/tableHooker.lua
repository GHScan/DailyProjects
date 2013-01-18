module(..., package.seeall)
--==================================================
local ListenerNode = {}
ListenerNode.__index = ListenerNode
ListenerNode.__newindex = ListenerNode
function ListenerNode.new(parent, k)
    local node = { parent = parent, children = {}, listeners = {}}
    if parent then parent.children[k] = node end
    setmetatable(node, ListenerNode)
    return node
end
function ListenerNode:getOrCreateChild(path)
    local node = self
    for k in string.gmatch(path, '[^%.]+') do
        local child = node.children[k]
        if not child then child = ListenerNode.new(node, k) end
        node = child
    end
    return node
end
function ListenerNode:getChild(path)
    local node = self
    for k in string.gmatch(path, '[^%.]+') do
        node = node.children[k]
        if not node then break end
    end
    return node
end
function ListenerNode:collectSelfAndParents(set)
    local node = self
    while node do
        set[node] = true
        node = node.parent
    end
end
function ListenerNode:collectSelfAndChildren(set)
    set[self] = true
    for _, v in pairs(self.children) do 
        v:collectSelfAndChildren(set)
    end
end
function ListenerNode:addListener(f)
    self.listeners[f] = true
end
function ListenerNode:removeListener(f)
    self.listeners[f] = nil
end
function ListenerNode:invokeListeners(set)
    for f in pairs(self.listeners) do set[f] = true end
end
--==================================================
local g_mt = {}

local function _getOrCreateProxy(parentProxy, key, listenerNode, realT)
    local r = parentProxy and parentProxy.__children[key]
    if not r then
        r = { __children = {}, __listenerNode = listenerNode,} 
        if parentProxy then
            parentProxy.__children[key] = r
            r.__root = parentProxy.__root
        else
            r.__root = r
            r.__activeListenerNodes = {}
            r.__enableAutoInvokeListeners = true
        end
    end
    r.__realT = realT
    setmetatable(r, g_mt)
    return r
end

g_mt.__index = function(t, k)
    local v = t.__realT[k]
    if type(v) == 'table' then
        v = _getOrCreateProxy(t, k, t.__listenerNode:getOrCreateChild(k), v)
    end
    return v
end
g_mt.__newindex = function(t, k, v)
    if t.__realT[k] ~= v then
        t.__realT[k] = v

        local node = t.__listenerNode
        node:collectSelfAndParents(t.__root.__activeListenerNodes)
        node = node:getChild(k)
        if node then 
            node:collectSelfAndChildren(t.__root.__activeListenerNodes) 
        end
        if t.__root.__enableAutoInvokeListeners then
            invokeActiveListeners(t.__root)
        end
    end
end
g_mt.__mode = 'v'

function addListener(t, path, f)
    t.__listenerNode:getOrCreateChild(path):addListener(f)
end
function removeListener(t, path, f)
    t.__listenerNode:getOrCreateChild(path):removeListener(f)
end
function invokeActiveListeners(t)
    local listeners = {}
    for node, _ in pairs(t.__activeListenerNodes) do
        node:invokeListeners(listeners)
    end
    t.__activeListenerNodes = {}
    for f in pairs(listeners) do
        f()
    end
end
function enableAutoInvokeListeners(t, enable)
    t.__enableAutoInvokeListeners = enable
end
function _pairs(t)
    local k = nil
    return function()
        k = next(t.__realT, k)
        return k, t[k]
    end
end
function hook(t)
    return _getOrCreateProxy(nil, '', ListenerNode.new(), t)
end
function unhook(t)
    return t.__realT
end
