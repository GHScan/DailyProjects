
function renderObject()
    a = nil
    a()
end
function render()
    renderObject()
end
function mainLoop()
    for i = 1, 10 do
        render()
    end
end

a = {b =_G}
a.b['mainLoop']()

