require "lfs"
require "luaxml"

local function verifyRawDataHeader(header)
	assert(header[1][1] == "日期")
	assert(header[2][1] == "比赛")
	assert(header[3][1] == "类型")
	assert(header[4][1] == "首发")
	assert(header[5][1] == "时间")
	assert(header[6][1] == "投篮")
	assert(header[7][1] == "三分")
	assert(header[8][1] == "罚球")
	assert(header[9][1] == "前篮板")
	assert(header[10][1] == "后篮板")
	assert(header[11][1] == "总篮板")
	assert(header[12][1] == "助攻")
	assert(header[13][1] == "抢断")
	assert(header[14][1] == "盖帽")
	assert(header[15][1] == "失误")
	assert(header[16][1] == "犯规")
	assert(header[17][1] == "得分")
end

local function verifyRawDataFormat(data)
	assert(string.match(data[1][1], "%d%d%d%d%-%d%d%-%d%d")) -- 日期
	assert(string.match(data[2][1][1], "%d%d%d?%-%d%d%d? .+")) -- 比分 对手
	assert(data[3][1] == "季后赛" or data[3][1] == "常规赛") -- 季后赛 or 常规赛
	assert(data[4][1] == "是" or data[4][1] == "")  -- 首发 or ""
	assert(string.match(data[5][1], "%d%d?")) -- 时间
	assert(string.match(data[6][1], "%d%d?%-%d%d?")) -- 命中/投篮
	assert(string.match(data[7][1], "%d%d?%-%d%d?")) -- 三分 命中/投篮
	assert(string.match(data[8][1], "%d%d?%-%d%d?")) -- 罚球 命中/投篮
	assert(string.match(data[9][1], "%d%d?")) -- 前场篮板
	assert(string.match(data[10][1], "%d%d?")) -- 后场篮板
	assert(string.match(data[11][1], "%d%d?")) -- 总篮板
	assert(string.match(data[12][1], "%d%d?")) -- 助攻
	assert(string.match(data[13][1], "%d%d?")) -- 抢断
	assert(string.match(data[14][1], "%d%d?")) -- 盖帽
	assert(string.match(data[15][1], "%d%d?")) -- 失误
	assert(string.match(data[16][1], "%d%d?")) -- 犯规
	assert(string.match(data[17][1], "%d%d?")) -- 得分
end

local function convertRawDataToDBData(data, extraData)
	if data == nil then return end
	assert(extraData)

	local ret = {}

	-- 这里是按数据库的格式填写, 所有项全是字符串!
	ret[1] = string.format("\"%s\"", data[1][1]) -- 日期
	if data[4][1] == "是" then ret[2] = "1" else ret[2] = "0" end -- 首发
	ret[3] = data[5][1]-- 时间
	ret[4] = string.match(data[7][1], "(%d%d?)%-%d%d?")-- 三分命中
	ret[5] = string.match(data[7][1], "%d%d?%-(%d%d?)")-- 三分出手
	ret[6] = tostring(tonumber(string.match(data[6][1], "(%d%d?)%-%d%d?")) - ret[4]) -- 两分命中
	ret[7] = tostring(tonumber(string.match(data[6][1], "%d%d?%-(%d%d?)")) - ret[5])-- 两分出手
	ret[8] = string.match(data[8][1], "(%d%d?)%-%d%d?")-- 罚球命中
	ret[9] = string.match(data[8][1], "%d%d?%-(%d%d?)")-- 罚球出手
	ret[10] = data[9][1]-- 前场篮板
	ret[11] = data[10][1]-- 后场篮板
	ret[12] = data[12][1]-- 助攻
	ret[13] = data[13][1]-- 抢断
	ret[14] = data[14][1]-- 盖帽
	ret[15] = data[15][1]-- 失误
	ret[16] = data[16][1]-- 犯规
	ret[17] = data[17][1]-- 得分
	ret[18] = string.format("\"%s\"", extraData[1]) -- 球队

	return ret
end

local function dbDataIter(doc)
	local extraData = {}
	extraData[1] = string.sub(doc:find("BODY")[2]:find("DIV", "ID", "main")[1][1][1][5][1], 1, -3)

	local t = doc:find("BODY")[2]:find("DIV", "ID", "main")[1][8][1][1]
	assert(#t > 1)
	verifyRawDataHeader(t[1])
	verifyRawDataFormat(t[2])
	local i = 1
	return
		function ()
			i = i + 1
			return convertRawDataToDBData(t[i], extraData)
		end
end

function captureDBDataFromNetwork(url)
	local http = require "socket.http"
	local xmlText = nil

	do
		local html = http.request(url)

		local f = io.open("input.html", "w")
		f:write(html)
		f:close()

		os.execute(lfs.currentdir() .. "\\html2xml\\html2xml.exe input.html input.xml")

		f = assert(io.open("input.xml", "r"))
		xmlText = f:read("*a")
		f:close()

		os.execute("del input.html")
		os.execute("del input.xml")
	end

	local doc = xml.eval(xmlText)[1] -- 跳过tags

	local ret = {}
	for data in dbDataIter(doc) do
		table.insert(ret, data)
	end

	return ret
end
