require "luasql.mysql"

local function executeWithHook(conn, sqlStr)
	--print(sqlStr)
	conn:execute(sqlStr)
end

function updatePlayerStatisticsToMysqlDB(name, data)
	if data == nil then return end
	assert(#data == 18)
	if tonumber(data[3]) == 0 then return end -- 没有出场

	local mysql = luasql.mysql()
	local conn = mysql:connect("nba", "root", "mysqlpassword", "localhost")
	conn:execute("set names gb2312")

	executeWithHook(conn, string.format("delete from 比赛统计 where 姓名=\"%s\" and 日期=%s", name, data[1]))
	executeWithHook(conn, string.format("insert into 比赛统计 values(\"%s\", %s)", name, table.concat(data, ",")))

	conn:close()
end
