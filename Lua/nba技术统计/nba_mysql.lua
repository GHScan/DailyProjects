require "luasql.mysql"

local function executeWithHook(conn, sqlStr)
	--print(sqlStr)
	conn:execute(sqlStr)
end

function updatePlayerStatisticsToMysqlDB(name, data)
	if data == nil then return end
	assert(#data == 18)
	if tonumber(data[3]) == 0 then return end -- û�г���

	local mysql = luasql.mysql()
	local conn = mysql:connect("nba", "root", "mysql19870615scan", "localhost")
	conn:execute("set names gb2312")

	executeWithHook(conn, string.format("delete from ����ͳ�� where ����=\"%s\" and ����=%s", name, data[1]))
	executeWithHook(conn, string.format("insert into ����ͳ�� values(\"%s\", %s)", name, table.concat(data, ",")))

	conn:close()
end
