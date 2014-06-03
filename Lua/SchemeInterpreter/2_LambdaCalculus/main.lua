function S_parse(s)
    s = string.gsub(s, ';[^\n]+\n', '')
    s = string.gsub(s, '%s+', ',')
    s = string.gsub(s, '[%(%)]', {['(']='{',[')']='}'})
    s = string.gsub(s, '[^{},%d][^{},]*', '"%1"')
    return assert(loadstring(string.format("return {%s}", s)))()[1]
end
function S_createLambda(env, argIdx, expArgs, expBody)
    return function(arg)
        local newEnv = {[expArgs[argIdx]]=arg}
        setmetatable(newEnv, {__index=env})
        if argIdx == #expArgs then
            for i = 3, #expBody - 1 do S_interpret(newEnv, expBody[i]) end
            return S_interpret(newEnv, expBody[#expBody])
        else
            return S_createLambda(newEnv, argIdx + 1, expArgs, expBody)
        end
    end
end
function S_interpret(env, exp)
    if type(exp) == 'string' then 
        return env[exp]
    elseif exp[1] == 'lambda' then
        return S_createLambda(env, 1, #exp[2] > 0 and exp[2] or {'_'}, exp)
    else 
        local p = S_interpret(env, exp[1])
        for i = 2, math.max(#exp, 2) do 
            p = p(exp[i] and S_interpret(env, exp[i]) or nil)
        end
        return p
    end
end
function S_createVM()
    return {
        G = {
            ['print'] = function(n) print(n(function(i) return i + 1 end)(0)) end,
        },
    }
end
function S_eval(vm, s)
    return S_interpret(vm.G, S_parse(s))
end

S_eval(S_createVM(), io.read('*a'))
