-- vim:fileencoding=gbk

function value2String(v)
    if type(v) == 'string' then
        return string.format('%q', v)
    elseif type(v) == 'table'  then
        local r = {'{'}
        for key, value in pairs(v) do
            table.insert(r, '[')
            table.insert(r, value2String(key))
            table.insert(r, ']=')
            table.insert(r, value2String(value))
            table.insert(r, ',')
        end
        table.insert(r, '}')
        return table.concat(r)
    else
        return tostring(v)
    end
end

function printValue(v)
    print(value2String(v))
end

identity = function() end
------------------------------

local jsonParser = (function()
    require 'PEG'
    setfenv(1, PEG)

    local array, dict, value, keyValuePair, STRING, INTEGER, FLOAT, BOOLEAN
    STRING = term('"[^"]*"')
    INTEGER = map(term('%d+'), tonumber)
    FLOAT = map(term('%d+%.%d+'), tonumber)
    BOOLEAN = map(term('true|false'), function(v) return v == 'true' end)
    array = lazy(function()
        return right(term('%['), 
        left(
        repsep(value, term(',')), 
        term('%]')))
    end)
    keyValuePair = lazy(function()
        return concat(left(STRING, term(':')), value)
    end)
    dict = lazy(function()
        return right(term('{'), 
        left(
        map(repsep(keyValuePair, term(',')), 
        function(l)
            local r = {}
            for _, p in ipairs(l) do
                r[p[1]] = p[2]
            end
            return r
        end), 
        term('}')))
    end)
    value = lazy(function()
        return choice(STRING,
        choice(FLOAT,
        choice(INTEGER,
        choice(BOOLEAN,
        choice(array,
        choice(dict,
        fail('Parse value failed')))))))
    end)

    return phrase(value)
end)()

------------------------------
local jsonStrs = {
    [==[23.5]==],
    [==[ "abcd efg fds" ]==],
    [==[ [1, 2, 3] ]==],
    [==[ [1, {"a":1, "b":2}, 3] ]==],
    [==[ [1, {"a":{"a":1, "b":"def"}, "b":[2,3]}, 3] ]==],
}
for _, s in ipairs(jsonStrs) do
    jsonParser(s, printValue, print)
end

local start = os.clock()
for i = 1, 1000 do
    for _, s in ipairs(jsonStrs) do
        jsonParser(s, identity, identity)
    end
end
print(os.clock() - start)
