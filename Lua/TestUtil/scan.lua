require "scan\\lfs_ext"
require "scan\\string_ext"
require "scan\\table_ext"

module(..., package.seeall)

-- 实现bind, 用法如boost::bind
local holderMap = {}
for i = 1, 9 do _G["_" .. i] = {} holderMap[_G["_" .. i]] = i end

local function bind_merge(topParams, params)
	local r = {}
	for i = 1, #topParams do
		local idx = holderMap[topParams[i]]
		if idx then
			r[i] = params[idx]
		else
			r[i] = topParams[i]
		end
	end
	return r
end

function bind(f, ...)
	local topParams = {...}
	return
		function(...)
			local params = {...}
			return f(unpack(bind_merge(topParams, params)))
		end
end

-- 扩展tostring
function tostring(t)
	if type(t) ~= "table" then
		return _G.tostring(t)
	else
		local ts = {}
		table.insert(ts, "{")
		for k, v in pairs(t) do
			table.insert(ts, _G.tostring(k) .. "=" .. tostring(v))
		end
		table.insert(ts, "}")
		return table.concat(ts, ",")
	end
end

-- 用法: for line in shell_lines("ls") do ... end
function shell_lines(cmd)
    local tmpName = os.tmpname()
    os.execute(string.format("(%s) > %s", cmd, tmpName))

    local lines = {}
    do
        local f = io.open(tmpName, "r")
        for line in f:lines() do
            table.insert(lines, line)
        end
        f:close()
    end

    os.execute(string.format("rm %s", tmpName))

    local i = 0
    return function()
        i = i + 1
        return lines[i]
    end
end

--用法: print(shell("ls"))
function shell(cmd)
    local t = {}
    for line in shell_lines(cmd) do
        table.insert(t, line)
    end
    return table.concat(t, "\n")
end
