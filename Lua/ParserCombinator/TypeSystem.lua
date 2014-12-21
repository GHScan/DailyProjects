
function makeClass(mtable)
    local oldNew = mtable.new
    assert(oldNew)
    mtable.new = function(class, ...)
        assert(class == mtable)
        local o = oldNew(class, ...)
        setmetatable(o, class)
        return o
    end

    if not mtable.__index then
        mtable.__index = mtable
    end
    return mtable
end

function isInstance(o, class)
    return getmetatable(o) == class
end
