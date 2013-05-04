
env = {
    __index = _G,
    math = {
        sqrt = function(a)
            return a^3
        end
    }
}
setmetatable(env, env)

function func()
    print(math.sqrt(2))
end

print(getfenv() == _G)
print(getfenv(func) == _G)

func()
setfenv(func, env)
func()

print(math.sqrt(2))
setfenv(1, env)
print(math.sqrt(2))
