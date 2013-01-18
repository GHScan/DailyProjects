require "nba_network"
require "nba_mysql"

local nbaPlayerList =
{
	["科比-布莱恩特"] = "http://nba.sports.sina.com.cn/star/Kobe-Bryant.shtml",
	["德维恩-韦德"] = "http://nba.sports.sina.com.cn/star/Dwyane-Wade.shtml",
	["勒布朗-詹姆斯"] = "http://nba.sports.sina.com.cn/star/LeBron-James.shtml",
}

for name, url in pairs(nbaPlayerList) do
	local dbDatas = captureDBDataFromNetwork(url)
	for _, data in ipairs(dbDatas) do
		updatePlayerStatisticsToMysqlDB(name, data)
	end
	print("处理完毕 : ", name)
end
