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
    while p ~= S_empty do
        p = S_cdr(p)
    end
    return p
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

function printTable(t)
    if type(t) == 'table' then
        io.write('(')
        for i, v in ipairs(t) do
            if i > 1 then io.write(' ') end
            printTable(v)
        end
        io.write(')')
    else
        io.write(t)
    end
end

function printSExp(e)
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

local function eval(env, exp)
    local expType = type(exp)
    if expType == 'boolean' or expType == 'number' then
        return exp
    elseif expType == 'string' then
        return env[exp]
    else
        if exp[1] == 'quote' then
            return array2SExp(exp[2])
        elseif exp[1] == 'if' then
            if eval(env, exp[2]) then
                return eval(env, exp[3])
            else
                return eval(env, exp[4])
            end
        elseif exp[1] == 'lambda' then
            return {env, exp[2], 1, exp, 3}
        elseif exp[1] == 'begin' then
            return evalSequentially(env, exp, 2)
        elseif exp[1] == 'cond' then
            local i = 2
            while i <= #exp do
                if eval(env, exp[i][1]) then
                    break
                end
                i = i + 1
            end
            assert(i <= #exp, 'cond should be end with else!')
            return evalSequentially(env, exp[i], 2)
        elseif exp[1] == 'define' then
            if type(exp[2]) == 'string' then
                rawset(env, exp[2], eval(env, exp[3]))
            else
                rawset(env, exp[2][1], {env, exp[2], 2, exp, 3})
            end
            return nil
        elseif exp[1] == 'set!' then
            env[exp[2]] = eval(env, exp[3])
            return nil
        elseif exp[1] == 'let' then
            local newExp = {'lambda', mapArray(arrayFirst, cloneArray(exp[2])) }
            for i = 3, #exp do
                newExp[#newExp + 1] = exp[i]
            end
            newExp = {newExp}
            for _, v in ipairs(mapArray(arraySecond, cloneArray(exp[2]))) do
                newExp[#newExp + 1] = v
            end
            return eval(env, newExp)
        else
            local f = eval(env, exp[1])
            local actuals = {}
            for i = 2, #exp do
                actuals[i - 1] = eval(env, exp[i])
            end
            if type(f) == 'table' then
                local newEnv = {}
                for i = f[3], #f[2] do
                    newEnv[f[2][i]] = actuals[i - f[3] + 1]
                end
                setmetatable(newEnv, {__index = f[1], __newindex = f[1]})
                return evalSequentially(newEnv, f[4], f[5])
            else
                return f(unpack(actuals))
            end
        end
    end
end

evalSequentially = function(env, seq, start)
    for i = start, #seq - 1 do
        eval(env, seq[i])
    end
    return eval(env, seq[#seq])
end

local G = {
    ['+'] = function(a, b) return a + b end, ['-'] = function(a, b) return a - b end,
    ['*'] = function(a, b) return a * b end, ['/'] = function(a, b) return a / b end,
    ['quotient'] = function(a, b) return math.floor(a / b) end, ['remainder'] = function(a, b) return a % b end,
    ['sqr'] = function(a) return a * a end, ['sqrt'] = function(a) return math.sqrt(a) end,
    ['identity'] = function(a) return a end, 
    ['true'] = true, ['false'] = false, ['else'] = true,
    ['='] = function(a, b) return a == b end, ['not'] = function(a) return not a end,
    ['<'] = function(a, b) return a < b end, ['<='] = function(a, b) return a <= b end,
    ['>'] = function(a, b) return a > b end, ['>='] = function(a, b) return a >= b end,
    ['cons'] = S_cons, ['car'] = S_car, ['cdr'] = S_cdr,
    ['cadr'] = S_cadr, ['caddr'] = S_caddr, ['cadddr'] = S_cadddr, 
    ['drop'] = S_drop, ['append'] = S_append, ['length'] = S_length, 
    ['empty'] = S_empty, ['empty?'] = function(a) return a == S_empty end,
    ['pretty-print'] = printSExp,
    ['current-inexact-milliseconds'] = function() return os.clock() * 1000 end,
    ['random'] = function(a) return math.random(a) - 1 end,
}
G['eval'] = function(e) return eval(G, SExp2Array(e)) end
------------------------------
for _, e in ipairs(parse(io.read('*a'))) do
    local v = eval(G, e)
    if v then printSExp(v) end
end
