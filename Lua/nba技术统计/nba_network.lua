require "lfs"
require "luaxml"

local function verifyRawDataHeader(header)
	assert(header[1][1] == "����")
	assert(header[2][1] == "����")
	assert(header[3][1] == "����")
	assert(header[4][1] == "�׷�")
	assert(header[5][1] == "ʱ��")
	assert(header[6][1] == "Ͷ��")
	assert(header[7][1] == "����")
	assert(header[8][1] == "����")
	assert(header[9][1] == "ǰ����")
	assert(header[10][1] == "������")
	assert(header[11][1] == "������")
	assert(header[12][1] == "����")
	assert(header[13][1] == "����")
	assert(header[14][1] == "��ñ")
	assert(header[15][1] == "ʧ��")
	assert(header[16][1] == "����")
	assert(header[17][1] == "�÷�")
end

local function verifyRawDataFormat(data)
	assert(string.match(data[1][1], "%d%d%d%d%-%d%d%-%d%d")) -- ����
	assert(string.match(data[2][1][1], "%d%d%d?%-%d%d%d? .+")) -- �ȷ� ����
	assert(data[3][1] == "������" or data[3][1] == "������") -- ������ or ������
	assert(data[4][1] == "��" or data[4][1] == "")  -- �׷� or ""
	assert(string.match(data[5][1], "%d%d?")) -- ʱ��
	assert(string.match(data[6][1], "%d%d?%-%d%d?")) -- ����/Ͷ��
	assert(string.match(data[7][1], "%d%d?%-%d%d?")) -- ���� ����/Ͷ��
	assert(string.match(data[8][1], "%d%d?%-%d%d?")) -- ���� ����/Ͷ��
	assert(string.match(data[9][1], "%d%d?")) -- ǰ������
	assert(string.match(data[10][1], "%d%d?")) -- ������
	assert(string.match(data[11][1], "%d%d?")) -- ������
	assert(string.match(data[12][1], "%d%d?")) -- ����
	assert(string.match(data[13][1], "%d%d?")) -- ����
	assert(string.match(data[14][1], "%d%d?")) -- ��ñ
	assert(string.match(data[15][1], "%d%d?")) -- ʧ��
	assert(string.match(data[16][1], "%d%d?")) -- ����
	assert(string.match(data[17][1], "%d%d?")) -- �÷�
end

local function convertRawDataToDBData(data, extraData)
	if data == nil then return end
	assert(extraData)

	local ret = {}

	-- �����ǰ����ݿ�ĸ�ʽ��д, ������ȫ���ַ���!
	ret[1] = string.format("\"%s\"", data[1][1]) -- ����
	if data[4][1] == "��" then ret[2] = "1" else ret[2] = "0" end -- �׷�
	ret[3] = data[5][1]-- ʱ��
	ret[4] = string.match(data[7][1], "(%d%d?)%-%d%d?")-- ��������
	ret[5] = string.match(data[7][1], "%d%d?%-(%d%d?)")-- ���ֳ���
	ret[6] = tostring(tonumber(string.match(data[6][1], "(%d%d?)%-%d%d?")) - ret[4]) -- ��������
	ret[7] = tostring(tonumber(string.match(data[6][1], "%d%d?%-(%d%d?)")) - ret[5])-- ���ֳ���
	ret[8] = string.match(data[8][1], "(%d%d?)%-%d%d?")-- ��������
	ret[9] = string.match(data[8][1], "%d%d?%-(%d%d?)")-- �������
	ret[10] = data[9][1]-- ǰ������
	ret[11] = data[10][1]-- ������
	ret[12] = data[12][1]-- ����
	ret[13] = data[13][1]-- ����
	ret[14] = data[14][1]-- ��ñ
	ret[15] = data[15][1]-- ʧ��
	ret[16] = data[16][1]-- ����
	ret[17] = data[17][1]-- �÷�
	ret[18] = string.format("\"%s\"", extraData[1]) -- ���

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

	local doc = xml.eval(xmlText)[1] -- ����tags

	local ret = {}
	for data in dbDataIter(doc) do
		table.insert(ret, data)
	end

	return ret
end
