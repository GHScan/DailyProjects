require "scan"

local srcPath = select(1, ...)
srcPath = lfs.normalize_path(srcPath)
local exts =
{
	cpp=true, h=true, c=true, cc=true, cpp=true,
}

print("输出有效化后的文件吗?[y/n]")
local genRawFile = io.read() == "y"
local tmpPath = ""
if genRawFile then
	print("输入临时文件夹名:")
	tmpPath = lfs.normalize_path(string.trim(io.read()))
	if not tmpPath or #tmpPath == 0 then tmpPath = "temp" end
	lfs.mkdir(tmpPath)
end

-- 基本行数
local lines = 0
-- 有效行数
local rawLines = 0
for file in lfs.dir_recursive(srcPath) do
	local dir, name, ext= lfs.split_path(file)
	if exts[ext] then
		local f = io.open(file, "r")
		if f then
			local content = f:read("*all")
			local _, n = string.gsub(content, "\n", "\n")
			lines = lines + n

			-- 去单行注释
			local rawContent = string.gsub(content, "//.-\n", "\n")
			-- 去多行注释
			rawContent = string.gsub(rawContent, "/%*.-%*/", "")
			-- 去多余的换行
			rawContent = string.gsub(rawContent, "\n[%c%s]*\n", "\n")
			_, n = string.gsub(rawContent, "\n", "\n")

			if genRawFile then
				local relativePath = string.sub(file, #srcPath + 1, -1)
				if relativePath and #relativePath > 0 then
					local destPath = tmpPath .. relativePath
					lfs.mkdir_recursive(destPath)
					local f = io.open(destPath, "w")
					if f then f:write(rawContent) end
				end
			end

			rawLines = rawLines + n
		end
	end
end

print("基本行                   : ", lines)
print("有效行(去注释和多余换行) : ", rawLines)
