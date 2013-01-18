require "util"

local srcPath = select(1, ...)

local f = io.open(srcPath, "r")
if not f then error("���ļ�ʧ��") end

local content = f:read("*all")

local tokens = {}
for token in string.gmatch(content, "[%a_][%w_]*") do
	tokens[token] = true
end

local dir, name, ext = splitPath(srcPath)
local destPath = dir .. "/" .. name .. ".tks"
fo = io.open(destPath, "w")
if not fo then error("������ļ�ʧ��") end

for i, v in table.opairs(tokens) do
	fo:write(i, "\n")
end

print("�����ɹ� :", destPath)
