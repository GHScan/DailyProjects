require "nba_network"
require "nba_mysql"

local nbaPlayerList =
{
	["�Ʊ�-��������"] = "http://nba.sports.sina.com.cn/star/Kobe-Bryant.shtml",
	["��ά��-Τ��"] = "http://nba.sports.sina.com.cn/star/Dwyane-Wade.shtml",
	["�ղ���-ղķ˹"] = "http://nba.sports.sina.com.cn/star/LeBron-James.shtml",
}

for name, url in pairs(nbaPlayerList) do
	local dbDatas = captureDBDataFromNetwork(url)
	for _, data in ipairs(dbDatas) do
		updatePlayerStatisticsToMysqlDB(name, data)
	end
	print("������� : ", name)
end
