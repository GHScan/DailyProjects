
function makeClass(mtable)
    if not mtable.__index then
        mtable.__index = mtable
    end
    return mtable
end

function isInstance(o, class)
    return getmetatable(o) == class
end
