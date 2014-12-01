-- vim:fileencoding=gbk

------------------------------
function Maybe(value) 
    return {
        flatMap = function(k)
            if value then
                return k(value)
            else
                return Maybe()
            end
        end,
        map = function(f)
            if value then
                return Maybe(f(value))
            else
                return Maybe()
            end
        end,
        filter = function(f)
            if value and f(value) then
                return Maybe(value)
            else
                return Maybe()
            end
        end,
        getValue = function()
            return value
        end,
        toString = function()
            return value and string.format('Just(%s)', value) or 'Nothing'
        end,
    }
end

function testMaybe()
    local r = 
    Maybe(3).flatMap(function(x)
        return 
        Maybe(4).filter(function(y)
            return x < y
        end).flatMap(function(y)
            return 
            Maybe(5).map(function(z)
                return x * y * z
            end)
        end)
    end)
    print(r.toString())
end
------------------------------
function List(a)
    return {
        flatMap = function(k)
            local newa = {}
            for _, v in ipairs(a) do
                for _, v2 in ipairs(k(v).getValue()) do
                    table.insert(newa, v2)
                end
            end
            return List(newa)
        end,
        map = function(f)
            local newa = {}
            for _, v in ipairs(a) do
                table.insert(newa, f(v))
            end
            return List(newa)
        end,
        filter = function(f)
            local newa = {}
            for _, v in ipairs(a) do
                if f(v) then
                    table.insert(newa, v)
                end
            end
            return List(newa)
        end,
        getValue = function()
            return a
        end,
        toString = function()
            local stra = {}
            for i, v in ipairs(a) do
                if i > 1 then table.insert(stra, ',') end
                table.insert(stra, v)
            end
            return table.concat(stra)
        end,
    }
end

function testList()
    local r = 
    List({1, 2, 3, 4, 5}).flatMap(function(x)
        return 
        List({1, 2, 3, 4, 5}).filter(function(y)
            return x < y
        end).flatMap(function(y)
            return 
            List({1, 2, 3, 4, 5}).filter(function(z)
                return y < z and (x * x + y * y == z * z)
            end).map(function(z)
                return x * y * z
            end)
        end)
    end)
    print(r.toString())
end
------------------------------
function Reader(reader)
    return {
        flatMap = function(k)
            return Reader(function(state) 
                return k(reader(state)).getValue()(state)
            end)
        end,
        map = function(f)
            return Reader(function(state)
                return f(reader(state))
            end)
        end,
        getValue = function()
            return reader
        end,
    }
end

function testReader()
    local read = function(k)
        return function(t)
            return t[k]
        end
    end

    local r = 
    Reader(read('x')).flatMap(function(x)
        return Reader(read('y')).map(function(y)
            return x * y
        end)
    end)
    print(r.getValue()({
        x = 2, y = 3
    }))
end
------------------------------
testMaybe()
testList()
testReader()
