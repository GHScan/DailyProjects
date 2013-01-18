require "scan"

local srcPath = select(1, ...)
srcPath = lfs.normalize_path(srcPath)
local exts =
{
	cpp=true, h=true, c=true, cc=true, cpp=true,
}

print("�����Ч������ļ���?[y/n]")
local genRawFile = io.read() == "y"
local tmpPath = ""
if genRawFile then
	print("������ʱ�ļ�����:")
	tmpPath = lfs.normalize_path(string.trim(io.read()))
	if not tmpPath or #tmpPath == 0 then tmpPath = "temp" end
	lfs.mkdir(tmpPath)
end

-- ��������
local lines = 0
-- ��Ч����
local rawLines = 0
for file in lfs.dir_recursive(srcPath) do
	local dir, name, ext= lfs.split_path(file)
	if exts[ext] then
		local f = io.open(file, "r")
		if f then
			local content = f:read("*all")
			local _, n = string.gsub(content, "\n", "\n")
			lines = lines + n

			-- ȥ����ע��
			local rawContent = string.gsub(content, "//.-\n", "\n")
			-- ȥ����ע��
			rawContent = string.gsub(rawContent, "/%*.-%*/", "")
			-- ȥ����Ļ���
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

print("������                   : ", lines)
print("��Ч��(ȥע�ͺͶ��໻��) : ", rawLines)
