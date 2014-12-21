module(..., package.seeall)

require "TypeSystem"
---------------------------------------------------
List = makeClass {
    new = function(class, value, next)
        return {value = value, next = next}
    end,
    items = function(class, ...)
        assert(class == List)
        local l = class.Nil
        local a = {...}
        for i = #a, 1, -1 do
            l = class:new(a[i], l)
        end
        return l
    end,

    isNil = function(self)
        return self == List.Nil
    end,
    toStringArray = function(self, a)
        if not self:isNil() then
            table.insert(a, tostring(self.value))
            self.next:toStringArray(a)
        end
    end,
    __tostring = function(self)
        local a = {}
        self:toStringArray(a)
        return string.format("List(%s)", table.concat(a, ","))
    end,

    map = function(self, f)
        if self:isNil() then
            return self
        else
            return List:new(f(self.value), self.next:map(f))
        end
    end,
    filter = function(self, f) 
        if self:isNil() then
            return self
        elseif f(self.value) then
            return List:new(self.value, self.next:filter(f))
        else
            return self.next:filter(f)
        end
    end,
    foldl = function(self, v, f)
        if self:isNil() then
            return v
        else
            return self.next:foldl(f(v, self.value), f)
        end
    end,
    __concat = function(self, l)
        if self:isNil() then
            return l
        else
            return List:new(self.value, self.next .. l)
        end
    end,
}
List.Nil = List:new(nil, nil)

Tuple = makeClass {
    new = function(class, ...)
        return {...}
    end,
    __tostring = function(self)
        local a = {}
        for _, v in ipairs(self) do
            table.insert(a, tostring(v))
        end
        return string.format("Tuple(%s)", table.concat(a, ','))
    end,
}
