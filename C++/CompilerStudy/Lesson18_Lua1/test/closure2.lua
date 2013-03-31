
function make(value)
    local add = function(off)
        value = value + off
    end
    local sub = function(off)
        value = value - off
    end
    local output = function()
        print(value)
    end
    return add, sub, output
end

local add, sub, output = make(10)
add(3)
add(10)
output()
sub(1)
output()
add(2)
sub(7)
output()
