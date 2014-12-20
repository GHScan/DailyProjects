module(..., package.seeall)

require "TypeSystem"

require "ParserCombinator"
local Parser = ParserCombinator.Parser
local Success = ParserCombinator.Success
local Failure = ParserCombinator.Failure
---------------------------------------------------
Location = makeClass {
    new = function(class, line, column)
        assert(class == Location)
        local a = {line = line, column = column}
        setmetatable(a, class)
        return a
    end,
    __tostring = function(self)
        return string.format("Location(%s,%s)", self.line, self.column)
    end,
}

Reader = makeClass {
    new = function(class, source, index, loc)
        assert(class == Reader)
        local a = {source = source, index = index, loc = loc}
        setmetatable(a, class)
        return a
    end,
    isEof = function(self)
        return self.index > #self.source
    end,
    advance = function(self, step)
        local line, column = self.loc.line, self.loc.column
        for i = 0, step - 1 do
            if self.source:byte(self.index + i) == 10 then
                line = line + 1
                column = 1
            else
                column = column + 1
            end
        end
        return Reader:new(self.source, self.index + step, Location:new(line, column))
    end,
    __tostring = function(self)
        return string.format("Reader(%s, %q )", tostring(self.loc), self.source:sub(self.index, 32))
    end,
}
---------------------------------------------------
local function buildParserFromString(s)
    return Parser:new(function(reader)
        if reader.source:sub(reader.index, reader.index + #s - 1) == s then
            return Success:new(s, reader:advance(#s))
        else
            return Failure:new("Expected: " .. s, reader)
        end
    end)
end

local function buildParserFromRegex(pattern)
    if pattern:sub(1, 1) ~= '^' then
        pattern = '^' .. pattern
    end
    return Parser:new(function(reader)
        local i, j = reader.source:find(pattern, reader.index)
        if i then
            return Success:new(reader.source:sub(i, j), reader:advance(j - i + 1))
        else
            return Failure:new("Expected: " .. pattern, reader)
        end
    end)
end

Parser.WhiteSpaceParser = buildParserFromRegex("%s+")

Parser.string = function(class, s)
    assert(class == Parser)
    return class.WhiteSpaceParser:opt():drop(buildParserFromString(s))
end

Parser.regex = function(class, pattern)
    assert(class == Parser)
    return class.WhiteSpaceParser:opt():drop(buildParserFromRegex(pattern))
end

Parser.phrase = function(self)
    return Parser:new(function(reader)
        return self:dropRight(Parser.WhiteSpaceParser:opt())(reader):bind(function(value, reader)
            if reader:isEof() then
                return Success:new(value, reader)
            else
                return Failure:new("Input is longer than a phrase...", reader)
            end
        end)
    end)
end

Parser.parseString = function(self, s)
    return self(Reader:new(s, 1, Location:new(1, 1)))
end
