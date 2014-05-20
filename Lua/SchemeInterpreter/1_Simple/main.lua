function S_source2Exp(s)
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
function S_call(vm, env, exp)
    local p = S_evalExp(vm, env, exp[1])
    if type(p) == 'function' then
        local args = {}
        for i = 2, #exp do args[i - 1] = S_evalExp(vm, env, exp[i]) end
        return p(unpack(args))
    else
        local newEnv = {[vm]=p.env}
        for i = 1, math.min(#exp - 1, #p.proto[2]) do 
            newEnv[p.proto[2][i]] = S_evalExp(vm, env, exp[i + 1]) 
        end
        for i = 3, #p.proto - 1 do 
            S_evalExp(vm, newEnv, p.proto[i]) 
        end
        return S_evalExp(vm, newEnv, p.proto[#p.proto])
    end
end
function S_evalExp(vm, env, exp)
    if type(exp) == 'number' then return exp 
    elseif type(exp) == 'string' then return S_lookupVar(vm, env, exp) 
    elseif vm.specialForms[exp[1]] then return vm.specialForms[exp[1]](vm, env, exp) 
    else return S_call(vm, env, exp) end
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
                return {proto=exp, env=env}
            end,
            ['if'] = function(vm, env, exp)
                if S_evalExp(vm, env, exp[2]) then
                    return S_evalExp(vm, env, exp[3])
                else
                    return S_evalExp(vm, env, exp[4])
                end
            end,
        },
    }
end
function S_eval(vm, s)
    return S_evalExp(vm, vm.G, S_source2Exp(s))
end

S_eval(S_createVM(), io.read('*a'))
