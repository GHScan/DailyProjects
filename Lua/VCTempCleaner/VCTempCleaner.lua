require "Util"

local garbageExts =
{
	"opt", "obj", "ilk",
	"plg", "aps", "bsc", "tmp", "trc",
	"pch", "idb", "exp", "sbr", "res",
}

local otherExts =
{
	"ncb", "pdb", "dll", "exe", "lib"
}

local ext2Files = {}
local srcPath = select(1, ...)

for path in subDir(srcPath, "f") do
	local dir, name, ext = splitPath(path)
	if dir and name and ext then
		ext2Files[ext] = ext2Files[ext] or {}
		table.insert(ext2Files[ext], path)
	end
end

for _, ext in ipairs(garbageExts) do
	local garbages = ext2Files[ext]
	if garbages then
		print("正在清理[", ext, "]...")
		for _, file in ipairs(garbages) do
			os.remove(file)
		end
	end
end

print("是否继续清理特殊文件?[y/n]")
if io.read() ~= "y" then
	return
end

for _, ext in ipairs(otherExts) do
	local garbages = ext2Files[ext]
	if garbages then
		print("是否要清理[", ext, "][y/n]?")
		if io.read() == "y" then
			for _, file in ipairs(garbages) do
				os.remove(file)
			end
		end
	end
end

print("处理完毕")
