function process_book_url(picServer, url)
    local r = os.execute(string.format("wget \"%s\" -O temp.txt", url))
    assert(r == 0, string.format("wget failed: %s", url))

    local dirName = nil
    for line in io.lines("temp.txt") do
        if dirName == nil then
            dirName = string.match(line, "<title>(.+)</title>")
            if dirName ~= nil then
                os.execute(string.format("mkdir \"%s\"", dirName))
            end
        else
            local picUrls = string.match(line, "var ArrayPhoto=new Array%((.+)%)")
            if picUrls ~= nil then
                local picUrlTable = {}
                for picUrl in string.gmatch(picUrls, "\"(.-)\",") do
                    table.insert(picUrlTable, picServer .. picUrl)
                end

                local f = io.open(string.format("%s/picUrlList.txt", dirName), "w")
                f:write(table.concat(picUrlTable, "\n"))
                f:close()

                f = io.open(string.format("%s/download.bat", dirName), "w")
                f:write("cat picUrlList.txt | xargs wget -c -N\n")
                f:write("rm -f picUrlList.txt\n")
                f:write("rm -f %0\n")
                f:close()
            end
        end
    end

    os.execute("rm -f temp.txt")
end

function process_index_url(picServer, idxUrl)
    local r = os.execute(string.format("wget \"%s\" -O temp.txt", idxUrl))
    assert(r == 0, string.format("wget failed: %s", idxUrl))

    local f = io.open("temp.txt")
    local content = f:read("*a")
    f:close()
    os.execute("rm -f temp.txt")

    local startIdx = string.find(content, "class=list_bg_table", 1, true)
    local endIdx = nil
    if startIdx ~= nil then
        endIdx = string.find(content, "</table", startIdx, true)
    end
    if startIdx ~= nil and endIdx ~= nil then
        local strContainBookUrl = string.sub(content, startIdx, endIdx)
        for bookUrl in string.gmatch(strContainBookUrl, "'(http://.-)'") do
            process_book_url(picServer, bookUrl)
        end
    end
end

print("输入'慢慢看'网站的图片服务器地址和漫画索引地址：")
print("例如：http://218.manmankan.com 和 http://www.manmankan.com/html/81/index.asp")
local picServer = io.read("*l")
local idxUrl = io.read("*l")
process_index_url(picServer, idxUrl)
