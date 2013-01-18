require "pack"

---[[
-- 压缩率稍高点的打包
local function packData(t)
	assert(#t > 0)
	local maxNum = t[1]
	table.foreach(t, function(_, v) if v > maxNum then maxNum = v end end)
	local fmt = "H"
	if maxNum > 65535 then fmt = "I" end

	local ret = {}
	ret[1] = fmt
	table.foreach(t, function(_, v) table.insert(ret, string.pack(fmt, v)) end)
	return table.concat(ret)
end

local function unpackData(s)
	local fmt = "H"
	if string.sub(s, 1, 1) ~= fmt then fmt = "I" end
	s = string.sub(s, 2, -1)

	assert(#s > 0)

	local ret = {}
	local offset = 1
	while true do
		local n, val = string.unpack(s, fmt, offset)
		if val == nil then break end
		table.insert(ret, val)
		offset = n
	end

	return ret
end
--]]

-- 简易的打包, 压缩率低点
--[[
local function packData(t)
	return table.concat(t, ",")
end

local function unpackData(s)
	local t = {}
	string.gsub(s, "%d+", function(k) table.insert(t, tonumber(k)) end)
	return t
end
--]]

-- 核心算法
function lzwEncode(src)
	assert(#src > 0)

	local dicNewIdx = 256
	local dic = {}
	for i = 0, 255 do
		dic[string.char(i)] = i
	end

	local ret = {}

	local prefix = string.sub(src, 1, 1)
	for i = 2, #src do
		byte = string.sub(src, i, i)

		local prefix_byte = prefix .. byte
		if dic[prefix_byte] then
			prefix = prefix_byte
		else
			dic[prefix_byte], dicNewIdx = dicNewIdx, dicNewIdx + 1
			table.insert(ret, dic[prefix])
			prefix = byte
		end
	end

	table.insert(ret, dic[prefix])

	return packData(ret)
end

function lzwDecode(src)
	local t = unpackData(src)

	local revDicNewIdx = 256
	local revDic = {}
	for i = 0, 255 do
		revDic[i] = string.char(i)
	end

	local ret = {}

	for i = 1, #t - 1 do
		local ti, tn = t[i], t[i + 1]

		if tn == revDicNewIdx then
			tn = ti
		end

		revDic[revDicNewIdx] = revDic[ti] .. string.sub(revDic[tn], 1, 1)
		revDicNewIdx = revDicNewIdx + 1

		table.insert(ret, revDic[ti])
	end

	table.insert(ret, revDic[t[#t]])

	return table.concat(ret)
end

-- 文件压缩
if arg[1] then
	local fileName = arg[1]
	if string.sub(fileName, -3, -1) == "pak" then
		-- 解压缩
		local f = io.open(fileName, "rb")
		local fo = io.open(string.sub(fileName, 1, -4) .. "rst", "wb")
		if f and fo then
			local s = f:read("*a")
			fo:write(lzwDecode(s))
			print("解压缩完毕")
		else
			print("打开文件失败!!!")
		end
	else
		-- 压缩
		local f = io.open(fileName, "rb")
		local fo = io.open(string.sub(fileName, 1, -4) .. "pak", "wb")
		if f and fo then
			local s = f:read("*a")
			local so = lzwEncode(s)
			fo:write(so)
			print("压缩完毕. 压缩率 = ", #so / #s)
		else
			print("打开文件失败!!!")
		end
	end

	return
end

-- 字符串压缩
print("输入要压缩的字符串")
while true do
	local i = io.read("*l")
	if i == "exit" then os.exit() end

	local m = lzwEncode(i)
	local o = lzwDecode(m)
	if i ~= o then print("失败 : " .. o)
	else print("成功 : 压缩率 - ", #m / #i) end
end
