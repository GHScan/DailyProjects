module(..., package.seeall)

require "TypeSystem"

require "Functional"
local List = Functional.List
---------------------------------------------------
Success = makeClass {
    new = function(class, value, reader)
        return {value = value, reader = reader}
    end,
    __tostring = function(self)
        return string.format("Success(%s)", tostring(self.value))
    end,
    bind = function(self, f)
        return f(self.value, self.reader)
    end,
}
Failure = makeClass {
    new = function(class, message, reader)
        return {message = message, reader = reader}
    end,
    __tostring = function(self)
        return string.format("Failure(%s, %s)", self.message, tostring(self.reader))
    end,
    bind = function(self, f)
        return self
    end,
}
Error = makeClass {
    new = function(class, message, reader)
        return {message = message, reader = reader}
    end,
    __tostring = function(self)
        return string.format("Error(%s, %s)", self.message, tostring(self.reader))
    end,
    bind = function(self, f)
        return self
    end,
}

local Packer
Packer = makeClass {
    new = function(class, first, second)
        return {first = first, second = second}
    end,
    unpackToArray = function(self, a, n)
        if n > 2 then
            self.first:unpackToArray(a, n - 1)
        else
            assert(n == 2)
            table.insert(a, self.first)
        end
        table.insert(a, self.second)
    end,
    unpack = function(self, n)
        local a = {}
        self:unpackToArray(a, n)
        return unpack(a)
    end,
    __tostring = function(self)
        return string.format("Packer(%s,%s)", tostring(self.first), tostring(self.second))
    end,
}

Parser = makeClass {
    new = function(class, f)
        assert(f)
        return {func = f}
    end,
    succ = function(class, value)
        assert(class == Parser)
        return Parser:new(function(reader)
            return Success:new(value, reader)
        end)
    end,
    fail = function(class, message)
        assert(class == Parser)
        return Parser:new(function(reader)
            return Failure:new(message, reader)
        end)
    end,
    err = function(class, message)
        assert(class == Parser)
        return Parser:new(function(reader)
            return Error:new(message, reader)
        end)
    end,

    __tostring = function(self)
        return string.format("Parser(%s)", tostring(self.func))
    end,
    __call = function(self, reader)
        return self.func(reader)
    end,

    bind = function(self, f)
        return Parser:new(function(reader)
            return self(reader):bind(function(value, reader)
                return f(value)(reader)
            end)
        end)
    end,
    map = function(self, f)
        return Parser:new(function(reader)
            return self(reader):bind(function(value, reader)
                return Success:new(f(value), reader)
            end)
        end)
    end,
    concatenate = function(self, p2)
        return Parser:new(function(reader)
            return self(reader):bind(function(value, reader)
                return p2(reader):bind(function(value2, reader)
                    return Success:new(Packer:new(value, value2), reader)
                end)
            end)
        end)
    end,
    alternate = function(self, p2)
        return Parser:new(function(reader)
            local result = self(reader)
            if isInstance(result, Failure) then
                return p2(reader)
            else
                return result
            end
        end)
    end,

    __add = function(self, p2)
        return self:concatenate(p2)
    end,
    __concat = function(self, p2)
        return self:alternate(p2)
    end,

    drop = function(self, p2)
        return (self + p2):map(function(p)
            local a, b = p:unpack(2)
            return b
        end)
    end,
    dropRight = function(self, p2)
        return (self + p2):map(function(p)
            local a, b = p:unpack(2)
            return a
        end)
    end,
    opt = function(self)
        return self:map(function(v) return List:items(v) end) 
               .. Parser:succ(List.Nil)
    end,
    rep = function(self)
        return self:bind(function(x)
            return self:rep():map(function(xs)
                return List:new(x, xs)
            end)
        end) 
        .. Parser:succ(List.Nil)
    end,
    rep1 = function(self)
        return (self + self:rep()):map(function(p)
            local x, xs = p:unpack(2)
            return List:new(x, xs)
        end)
    end,
    chainl1 = function(self, elem, sep)
        return (self + (sep + elem):rep()):map(function(p)
            local v, xs = p:unpack(2)
            return xs:foldl(v, function(v, x)
                local op, xv = x:unpack(2)
                return op(v, xv)
            end)
        end)
    end,
    repsep1 = function(self, sep)
        return self:map(function(v)
            return List:items(v)
        end):chainl1(self, sep:map(function()
            return function(xs, x)
                return xs .. List:items(x)
            end
        end))
    end,
    repsep = function(self, sep)
        return self:repsep1(sep) .. Success:new(List.Nil)
    end,
}

ProxyParser = makeClass {
    new = function(class)
        return {}
    end,
    __call = function(self, reader)
        return self.parser(reader)
    end,
    __index = Parser,
}
