
function printPrime(n)
    local start = os.clock()
    for i = 2, n do
        local flag = 1
        for j = 2, i / 2 do
            if i % j == 0 then
                flag = 0
                break
            end
        end
        if flag then end
    end
    print("printPrime: ", os.clock() - start)
end
function perform()
    local start = os.clock()
    for i = 0, 1000000 do end
    print("perform: ", os.clock() - start)
end
function perform_1_5()
    local start = os.clock()
    local i = 0
    while i < 1000000 do i = i + 1 end
    print("perform 1.5: ", os.clock() - start)
end
function perform2()
    local start = os.clock()
    for i = 0, 100 do
        for j = 0, 1000 do
            for k = 0, 1000 do
            end 
        end 
    end
    print("perform2: ", os.clock() - start)
end
function feb(n)
    if n <= 2 then return 1 end
    return feb(n - 1) + feb(n - 2)
end
function perform3()
    local start = os.clock()
    for i = 0, 30 do feb(i) end
    print("perform3: ", os.clock() - start)
end
function main() 
    printPrime(10000)
    perform()
    perform_1_5()
    perform2()
    perform3()
end
main()
