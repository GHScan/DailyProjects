local mocPath = "F:\\Libraries\\Qt\\4_6_3\\bin\\moc.exe"
if string.sub(mocPath, -7, -1) ~= "moc.exe" or not io.open(mocPath, "rb") then
	error("�Ҳ���moc·��! ����config.lua������!!!")
end

local srcPath = select(1, ...)
if not io.open(srcPath, "rb") then
	error("�Ҳ���Դ�ļ�")
end

local srcDir = ""
local srcFileName = ""
do
	local sepPos = #srcPath - string.find(string.reverse(srcPath), "\\") + 1
	srcDir = string.sub(srcPath, 1, sepPos)
	srcFileName = string.sub(srcPath, sepPos + 1, -1)
end

local destDir = srcDir .. "moc\\"
os.execute("mkdir " .. destDir)

local destPath = destDir .. srcFileName .. ".cpp"

local cmd = mocPath .. " -o " .. destPath .. " " .. srcPath

os.execute(cmd)

print("ת���ɹ�")
