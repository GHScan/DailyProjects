------------------------------
local function mapArray(f, a)
    for i = 1, #a do
        a[i] = f(a[i])
    end
    return a
end

local function cloneArray(a)
    local r = {}
    for i = 1, #a do
        r[i] = a[i]
    end
    return r
end

local function arrayFirst(a)
    return a[1]
end

local function arraySecond(a)
    return a[2]
end

local function identity(a)
    return a
end
------------------------------
local S_empty = {}
local pairTag = {}

local function S_pair(p) return type(p) == 'table' and p[3] == pairTag end
local function S_cons(a, b) return {a, b, pairTag} end
local function S_car(p) return p[1] end
local function S_cdr(p) return p[2] end

local function S_cadr(p) return p[2][1] end
local function S_caddr(p) return p[2][2][1] end
local function S_cadddr(p) return p[2][2][2][1] end


local function arrayFromAppendArraySlist(a, l)
    while l ~= S_empty do
        a[#a + 1], l = S_car(l), S_cdr(l)
    end
    return a
end
local function slistFromAppendArraySlist(a, l)
    for i = #a, 1, -1 do
        l = S_cons(a[i], l)
    end
    return l
end


local function S_append(l1, l2)
    return slistFromAppendArraySlist(
        arrayFromAppendArraySlist(arrayFromAppendArraySlist({}, l1), l2), 
        S_empty)
end
local function S_drop(p, n)
    for i = 1, n do
        p = S_cdr(p)
    end
    return p
end
local function S_length(p)
    n = 0
    while p ~= S_empty do
        p = S_cdr(p)
        n = n + 1
    end
    return n
end


local function SExp2Array(exp)
    if S_pair(exp) then
        return mapArray(SExp2Array, arrayFromAppendArraySlist({}, exp))
    else
        return exp
    end
end

local function array2SExp(t)
    if type(t) == 'table' then
        return mapArray(array2SExp, cloneArray(t))
    else
        return t
    end
end

local function printTable(t)
    if type(t) == 'table' then
        io.write('(')
        for i, v in ipairs(t) do
            if i > 1 then io.write(' ') end
            printTable(v)
        end
        io.write(')')
    else
        io.write(tostring(t))
    end
end

local function printSExp(e)
    printTable(SExp2Array(e))
    print()
end
------------------------------
local function parse(s)
    s = string.gsub(s, ';[^\n]*\n', '')
    s = string.gsub(s, '[%(%)%[%]]', {['(']='{',[')']='} ',['[']='{',[']']='} '})
    s = string.gsub(s, '%s+', ',')
    s = string.gsub(s, '[^{},%d][^{},]*', '"%1"')
    return assert(loadstring(string.format("return {%s}", s)))()
end

local evalSequentially
local evalList

local function eval(env, exp, k)
    local expType = type(exp)
    if expType == 'string' then
        return k(env[exp])
    elseif expType ~= 'table' then
        return k(exp)
    elseif exp[1] == 'quote' then
        return k(array2SExp(exp[2]))
    elseif exp[1] == 'if' then
        return eval(env, exp[2], function(b) 
            if b then 
                return eval(env, exp[3], k)
            else 
                return eval(env, exp[4], k) 
            end
        end)
    elseif exp[1] == 'lambda' then
        return k(function(actuals, k)
            local newEnv = {}
            for i = 1, #exp[2] do
                newEnv[exp[2][i]] = actuals[i]
            end
            setmetatable(newEnv, {__index = env, __newindex = env})
            return evalSequentially(newEnv, exp, 3, k)
        end)
    elseif exp[1] == 'begin' then
        return evalSequentially(env, exp, 2, k)
    elseif exp[1] == 'cond' then
        if #exp == 2 then
            return evalSequentially(env, exp[2], 1, k)
        else
            local caseBody = cloneArray(exp[2])
            caseBody[1] = 'begin'
            local restCase = cloneArray(exp)
            table.remove(restCase, 2)
            local newExp = {'if', exp[2][1], caseBody, restCase}
            return eval(env, newExp, k)
        end
    elseif exp[1] == 'define' then
        if type(exp[2]) == 'string' then
            return eval(env, exp[3], function(v)
                rawset(env, exp[2], v)
                return k(nil)
            end)
        else
            local newExp = cloneArray(exp)
            newExp[1] = 'lambda'
            newExp[2] = cloneArray(newExp[2])
            table.remove(newExp[2], 1)
            newExp = {'define', exp[2][1], newExp}
            return eval(env, newExp, k)
        end
    elseif exp[1] == 'set!' then
        return eval(env, exp[3], function(v)
            env[exp[2]] = v
            return k(nil)
        end)
    elseif exp[1] == 'let' then
        local newExp = {'lambda', mapArray(arrayFirst, cloneArray(exp[2])) }
        for i = 3, #exp do
            newExp[#newExp + 1] = exp[i]
        end
        newExp = {newExp}
        for _, v in ipairs(mapArray(arraySecond, cloneArray(exp[2]))) do
            newExp[#newExp + 1] = v
        end
        return eval(env, newExp, k)
    else
        return eval(env, exp[1], function(f)
            return evalList(env, exp, 2, function(actuals)
                return f(actuals, k)
            end)
        end)
    end
end

evalSequentially = function(env, seq, start, k)
    if start == #seq then
        return eval(env, seq[start], k)
    else
        return eval(env, seq[start], function(_)
            return evalSequentially(env, seq, start + 1, k)
        end)
    end
end

evalList = function(env, list, start, k, result, i)
    result = result or {}
    i = i or 1
    if start > #list then
        return k(result)
    else
        return eval(env, list[start], function(v)
            result[i] = v
            return evalList(env, list, start + 1, k, result, i + 1)
        end)
    end
end

local function wrapFuncToCPS(f)
    return function(actuals, k)
        return k(f(unpack(actuals)))
    end
end

local G = {
    ['+'] = function(a, b) return a + b end, ['-'] = function(a, b) return a - b end,
    ['*'] = function(a, b) return a * b end, ['/'] = function(a, b) return a / b end,
    ['quotient'] = function(a, b) return math.floor(a / b) end, ['remainder'] = function(a, b) return a % b end,
    ['sqr'] = function(a) return a * a end, ['sqrt'] = function(a) return math.sqrt(a) end,
    ['identity'] = identity,
    ['true'] = true, ['false'] = false, ['else'] = true,
    ['='] = function(a, b) return a == b end, ['not'] = function(a) return not a end,
    ['<'] = function(a, b) return a < b end, ['<='] = function(a, b) return a <= b end,
    ['>'] = function(a, b) return a > b end, ['>='] = function(a, b) return a >= b end,
    ['cons'] = S_cons, ['car'] = S_car, ['cdr'] = S_cdr,
    ['cadr'] = S_cadr, ['caddr'] = S_caddr, ['cadddr'] = S_cadddr, 
    ['drop'] = S_drop, ['append'] = S_append, ['length'] = S_length, 
    ['empty'] = S_empty, ['empty?'] = function(a) return a == S_empty end,
    ['pretty-print'] = printSExp, ['display'] = function(e) return io.write(tostring(e)) end,
    ['current-inexact-milliseconds'] = function() return os.clock() * 1000 end,
    ['random'] = function(a) return math.random(a) - 1 end,
    ['exit'] = os.exit,
}
G['eval'] = function(e) return eval(G, SExp2Array(e), identity) end

for k, v in pairs(G) do
    if type(v) == 'function' then
        v = wrapFuncToCPS(v)
    end
    G[k] = v
end

G['call/cc'] = function(actuals, k) 
    return actuals[1]({
        function(actuals2, k2) 
            return k(actuals2[1])
        end}, 
    k) 
end
------------------------------
for _, e in ipairs(parse(io.read('*a'))) do
    local v = eval(G, e, identity)
    if v ~= nil then printSExp(v) end
end
