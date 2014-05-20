function S_parse(s)
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
    if type(exp) == 'number' then return exp 
    elseif type(exp) == 'string' then return S_lookupVar(vm, env, exp) 
    elseif vm.specialForms[exp[1]] then return vm.specialForms[exp[1]](vm, env, exp) 
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
            ['+'] = function(a, b) return a + b end,
            ['-'] = function(a, b) return a - b end,
            ['='] = function(a, b) return a == b end,
            ['print'] = print,
        },
        specialForms = {
            ['lambda'] = function(vm, env, exp)
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
            end,
            ['if'] = function(vm, env, exp)
                if S_interpret(vm, env, exp[2]) then
                    return S_interpret(vm, env, exp[3])
                else
                    return S_interpret(vm, env, exp[4])
                end
            end,
        },
    }
end
function S_eval(vm, s)
    return S_interpret(vm, vm.G, S_parse(s))
end

S_eval(S_createVM(), io.read('*a'))
