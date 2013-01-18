require "socket"

skt_server = socket.tcp()
assert(skt_server:bind("*", 2500))
assert(skt_server:listen(5))
print("server startup:", skt_server:getsockname())

read_skts = {}
write_skts = {}
skt_threads = {}

function skt_read_func(skt, func, ...)
    table.insert(read_skts, skt)
    coroutine.yield()
    local pos = nil
    for i = 1, #read_skts do 
        if read_skts[i] == skt then
            pos = i
            break
        end
    end
    table.remove(read_skts, pos)
    return skt[func](skt, ...)
end

function skt_write_func(skt, func, ...)
    skt[func](skt, ...)
    table.insert(write_skts, skt)
    coroutine.yield()
    local pos = nil
    for i = 1, #write_skts do 
        if write_skts[i] == skt then
            pos = i
            break
        end
    end
    table.remove(write_skts, pos)
end

function client_thread(skt)
    print("connection open:", skt:getpeername())
    while true do
        local msg, err = skt_read_func(skt, "receive", "*l")
        if err ~= nil then 
            print("receive error : ", err, skt:getpeername())
            break
        else
            if msg == "close" then
                break
            else
                print("receive msg:", msg, "-", skt:getpeername())
                skt_write_func(skt, "send", string.format("include <%s>\r\n", msg))
            end
        end
    end
    print("connection closed:", skt:getpeername())

    skt_threads[skt] = nil
    skt:close()
end

function server_thread()
    while true do
        local skt_client = skt_read_func(skt_server, "accept")

        skt_threads[skt_client] = coroutine.create(client_thread)
        coroutine.resume(skt_threads[skt_client], skt_client)
    end
end

skt_threads[skt_server] = coroutine.create(server_thread)
coroutine.resume(skt_threads[skt_server])

while true do
    local sub_read_skts, sub_write_skts, err = socket.select(read_skts, write_skts, 10)
    if err ~= nil then 
        if err == "timeout" then
            print("long time have not response, exit? (y/n)")
            if io.read("*l") == "y" then 
                break
            end
        else
            print("select error : ", err)
            break
        end
    end

    for _, skt in ipairs(sub_read_skts) do
        coroutine.resume(skt_threads[skt])
    end
    for _, skt in ipairs(sub_write_skts) do
        coroutine.resume(skt_threads[skt])
    end
end

table.foreach(skt_threads, function(k, v) k:close() end)
