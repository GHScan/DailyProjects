function S_parse(s)
    s = string.gsub(s, ';[^\n]+\n', '')
    s = string.gsub(s, '%s+', ',')
    s = string.gsub(s, '[%(%)]', {['(']='{',[')']='}'})
    s = string.gsub(s, '[^{},%d][^{},]*', '"%1"')
    return assert(loadstring(string.format("return {%s}", s)))()[1]
end
function S_lookupVar(vm, env, name)
    while env do
        if env[name] then return env[name] end
        env = env[vm]
    end
end
function S_interpret(vm, env, exp)
    if type(exp) == 'string' then return S_lookupVar(vm, env, exp) 
    elseif exp[1] == 'lambda' then
        return function(...)
            local newEnv = {[vm]=env}
            for i = 1, #exp[2] do
                newEnv[exp[2][i]] = arg[i]
            end
            for i = 3, #exp - 1 do
                S_interpret(vm, newEnv, exp[i])
            end
            return S_interpret(vm, newEnv, exp[#exp])
        end
    else 
        local p = S_interpret(vm, env, exp[1])
        local args = {}
        for i = 2, #exp do args[i - 1] = S_interpret(vm, env, exp[i]) end
        return p(unpack(args))
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
    return S_interpret(vm, vm.G, S_parse(s))
end

S_eval(S_createVM(), io.read('*a'))
