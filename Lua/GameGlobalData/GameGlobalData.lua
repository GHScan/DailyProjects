--==================================================
--- ListenerNode
--==================================================
local ListenerNode = {}
ListenerNode.__index = ListenerNode
function ListenerNode.New(parent, k)
    local node = { parent = parent, children = {}, listeners = {}}
    if parent then parent.children[k] = node end
    setmetatable(node, ListenerNode)
    return node
end
function ListenerNode:GetChild(path)
    local node = self
    for k in string.gmatch(path, '[^%.]+') do
        node = node.children[k]
        if not node then break end
    end
    return node
end
function ListenerNode:GetOrCreateChild(path)
    local node = self
    for k in string.gmatch(path, '[^%.]+') do
        local child = node.children[k]
        if not child then child = ListenerNode.New(node, k) end
        node = child
    end
    return node
end
function ListenerNode:GetExistChild(path)
    local node = self
    for k in string.gmatch(path, '[^%.]+') do
        local child = node.children[k]
        if not child then break end
        node = child
    end
    return node
end
function ListenerNode:MarkSelfAndParents(set)
    local node = self
    while node do
        set[node] = true
        node = node.parent
    end
end
function ListenerNode:MarkSelfAndChildren(set)
    set[self] = true
    for _, v in pairs(self.children) do 
        v:MarkSelfAndChildren(set)
    end
end
function ListenerNode:AddListener(f)
    self.listeners[f] = true
end
function ListenerNode:RemoveListener(f)
    self.listeners[f] = nil
end
function ListenerNode:MarkListeners(set)
    for f in pairs(self.listeners) do set[f] = true end
end
--==================================================
-- GlobalData
--==================================================
GameGlobalData = {
    m_rootListenerNode = ListenerNode.New(),
    m_activeListenerNodes = {},
    m_data = {},
}
--==================================================
-----------> public methods
--==================================================
function GameGlobalData:Merge(path, src)
    src = self:_ConvertArray2Table(src)

    local data, k = self:_Split2DataKey(path)
    if not data[k] then
        data[k] = tableext.rcopy(src)
        self:NotifyDataChange(path)
    else
        local listenerNode = self.m_rootListenerNode:GetChild(path)
        if self:_Merge(data[k], src, listenerNode) then
            listenerNode = listenerNode or self.m_rootListenerNode:GetExistChild(path)
            listenerNode:MarkSelfAndParents(self.m_activeListenerNodes)
        end
    end
end
function GameGlobalData:Delete(path)
    local data, k = self:_Split2DataKey(path)
    data[k] = nil
    self:NotifyDataChange(path)
end
function GameGlobalData:GetData(path)
    local data = self.m_data
    for k in string.gmatch(path, '[^%.]+') do
        data = data[k]
    end
    return data
end
function GameGlobalData:NotifyDataChange(path)
    local node = self.m_rootListenerNode:GetChild(path)
    if node then
        node:MarkSelfAndParents(self.m_activeListenerNodes)
        node:MarkSelfAndChildren(self.m_activeListenerNodes)
    else
        node = self.m_rootListenerNode:GetExistChild(path)
        node:MarkSelfAndParents(self.m_activeListenerNodes)
    end
end
function GameGlobalData:AddListener(path, f)
    self.m_rootListenerNode:GetOrCreateChild(path):AddListener(f)
end
function GameGlobalData:RemoveListener(path, f)
    self.m_rootListenerNode:GetChild(path):RemoveListener(f)
end
function GameGlobalData:ConsumeActiveListeners()
    local listeners = {}
    for node in pairs(self.m_activeListenerNodes) do
        node:MarkListeners(listeners)
    end
    self.m_activeListenerNodes = {}
    for f in pairs(listeners) do
        f()
    end
end
--==================================================
-----------> private methods
--==================================================
function GameGlobalData:_ConvertArray2Table(t)
    local r = {}
    local convert = tableext.isarray(t) and type(t[1]) == 'table' and t[1].uid
    for k, v in pairs(t) do
        if type(v) == 'table' then
            r[convert and tostring(v.uid) or k] = self:_ConvertArray2Table(v)
        else
            r[k] = v
        end
    end
    return r
end
function GameGlobalData:_Merge(dest, src, listenerNode)
    local changed = false
    for k, v in pairs(src) do
        if type(v) == 'table' then
            local childNode = listenerNode and listenerNode.children[k]
            if not dest[k] then
                dest[k] = tableext.rcopy(v)
                if childNode then
                    childNode:MarkSelfAndChildren(self.m_activeListenerNodes)
                end
                changed = true
            else
                changed = self:_Merge(dest[k], v, childNode) or changed
            end
        else
            if dest[k] ~= v then
                dest[k] = v
                changed = true
            end
        end
    end
    if changed and listenerNode then
        self.m_activeListenerNodes[listenerNode] = true
    end
    return changed
end
function GameGlobalData:_Split2DataKey(path)
    local prefix, k = unpack(stringext.splitext(path))
    local data = #prefix > 0 and self:GetData(prefix) or self.m_data
    return data, k
end
