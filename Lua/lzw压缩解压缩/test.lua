require "pack"

---[[
-- ѹ�����Ըߵ�Ĵ��
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

-- ���׵Ĵ��, ѹ���ʵ͵�
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

-- �����㷨
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

-- �ļ�ѹ��
if arg[1] then
	local fileName = arg[1]
	if string.sub(fileName, -3, -1) == "pak" then
		-- ��ѹ��
		local f = io.open(fileName, "rb")
		local fo = io.open(string.sub(fileName, 1, -4) .. "rst", "wb")
		if f and fo then
			local s = f:read("*a")
			fo:write(lzwDecode(s))
			print("��ѹ�����")
		else
			print("���ļ�ʧ��!!!")
		end
	else
		-- ѹ��
		local f = io.open(fileName, "rb")
		local fo = io.open(string.sub(fileName, 1, -4) .. "pak", "wb")
		if f and fo then
			local s = f:read("*a")
			local so = lzwEncode(s)
			fo:write(so)
			print("ѹ�����. ѹ���� = ", #so / #s)
		else
			print("���ļ�ʧ��!!!")
		end
	end

	return
end

-- �ַ���ѹ��
print("����Ҫѹ�����ַ���")
while true do
	local i = io.read("*l")
	if i == "exit" then os.exit() end

	local m = lzwEncode(i)
	local o = lzwDecode(m)
	if i ~= o then print("ʧ�� : " .. o)
	else print("�ɹ� : ѹ���� - ", #m / #i) end
end
