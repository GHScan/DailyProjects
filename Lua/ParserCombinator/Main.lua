-- vim:fileencoding=gbk
require "TypeSystem"

require "Functional"
local List = Functional.List
local Tuple = Functional.Tuple

require "ParserCombinator"
local Parser = ParserCombinator.Parser
local ProxyParser = ParserCombinator.ProxyParser

require "RegexParserCombinator"
-------------------------------------------------
local JsonParser
JsonParser = makeClass {
    new = function(class)
        assert(class == JsonParser)

        local valueParser = ProxyParser:new()
        local stringParser = Parser:regex([["[^"]*"]])
        local intParser = Parser:regex("%d+")
        local keyValueParser = (stringParser + Parser:string(":") + valueParser):map(function(p)
            local key, _, value = p:unpack(3)
            return Tuple:new(key, value)
        end)
        local arrayParser = (Parser:string("[") + valueParser:repsep(Parser:string(",")) + Parser:string("]")):map(function(p)
            local _, values, _ = p:unpack(3)
            return values
        end)
        local dictParser = (Parser:string("{") + keyValueParser:repsep(Parser:string(",")) + Parser:string("}")):map(function(p)
            local _, pairs, _ = p:unpack(3)
            return pairs
        end)
        valueParser.parser = stringParser .. intParser .. arrayParser .. dictParser

        local a = { parser = valueParser:phrase() }
        setmetatable(a, class)
        return a
    end,
    parse = function(self, input)
        return self.parser:parseString(input)
    end,
}
-------------------------------------------------
local jsonStrs = {
    [[234]],
    [[ "afsdjk" ]],
    [[ [3,2,"fads",4]  ]],
    [[{"a" : 1  , "b":2,  "c ":12}  ]],
    [[{"a":1,"b":2, "c" : [1,2,{"d":4,"e":5}]}  ]],
}
for _, s in ipairs(jsonStrs) do
    print(JsonParser:new():parse(s))
end
