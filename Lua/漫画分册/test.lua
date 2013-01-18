require "scan"

local MAX_PIC = 100

local srcDir = [[E:\漫画\_海贼王]]
local destDir = [[E:\漫画\海贼王]]

local curBook = 1
local curPic = 1

local lastPicDir = nil
for picName in scan.shell_lines(string.format([[find "%s" -type f]], srcDir)) do

    local curPicDir = string.format([[%s\%d]], destDir, curBook)
    if curPicDir ~= lastPicDir then
        lastPicDir = curPicDir
        os.execute(string.format([[mkdir "%s"]], curPicDir))
    end

    local ext = string.match(picName, [[(%.%w+)$]])
    local cmd = string.format([[cp "%s" "%s\%d%s"]], picName, curPicDir, curPic, ext)
    print(cmd)
    os.execute(cmd)

    curPic = curPic + 1
    if curPic > MAX_PIC then 
        curPic = 1
        curBook = curBook + 1
    end
end
