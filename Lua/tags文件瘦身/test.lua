require "scan"
--tags�ļ������������һ��token�Ķ���֮��ֻ���ļ�����ͬ���򿳵�������ֵ�
io.output("new.tags")
headerLine = 6
temp = {}
for i = 1, 20 do temp[i] = {} end
lastToken = nil
for line in io.lines("tags") do
    if headerLine > 0 then
        headerLine = headerLine - 1
        io.write(line, "\n")
    else
        local t = string.split(line, "\t")
        if t[1] ~= lastToken then
            lastToken = t[1]
            for i = 1, 20 do temp[i] = {} end
            for i = 3, #t do
                temp[i][t[i]] = true
            end

            io.write(line, "\n")
        else
            local exists = true
            for i = 3, #t do
                if not temp[i][t[i]] then
                    exists = false
                    break
                end
            end
            if not exists then
                for i = 3, #t do
                    temp[i][t[i]] = true
                end
                io.write(line, "\n")
            end
        end
    end
end
