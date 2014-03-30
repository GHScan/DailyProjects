function withEmpty()
    local start = os.clock()
    while true do
        local line = io.read('*l')
        if not line then break end
    end
    print(os.clock() - start)
    os.execute('sleep 30')
end

function withTable()
    local start = os.clock()
    local d = {}
    while true do
        local line = io.read('*l')
        if not line then break end
        d[line] = (d[line] or 0) + 1
    end
    print(os.clock() - start)
    os.execute('sleep 30')
end

_G['with' .. arg[1]]()
