function echo(str, b)
    print('echo', str)
    return b
end
function testAnd()
    print('jfkd')
    print('========== testAnd 1 ==========')
    local b = echo('1', true) and echo('1', true)
    print('========== testAnd 2 ==========')
    b = echo('0', false) and echo('1', true)
end
function testOr()
    print('========== testOr 1 ==========')
    local b = echo('0', false) or echo('0', false)
    print('========== testOr 1 ==========')
    b = echo('1', true) or echo('0', false)
end
function printPrime(n)
    print('========== printPrime ==========')
    local i, j
    for i = 2, n do
        local flag = true
        for j = 2, i / 2 do
            if i % j == 0 then
                flag = false
                break
            end
        end
        if flag then print(i) end
    end
    print()
end
function factorial(n)
    if n <= 1 then return 1 end
    return n * factorial(n - 1)
end
function printFeb1()
    print('========== printFeb1 ==========')
    local i, j = 1, 1
    while i < 100 do
        print(j);
        i, j = i + j, i
    end
    print()
end
function feb2(n)
    -- it's hard to print function name, so...
    if n <= 2 then return 1 end
    return feb2(n - 1) + feb2(n - 2)
end
function printFeb2()
    print('========== printFeb2 ==========')
    local i = 1
    repeat 
        print(feb2(i)) 
        i = i + 1
    until i > 10
    print()
end
function print9x9()
    print('========== print9x9 ==========')
    for i = 1, 9 do
        for j = 1, i do
            io.write(string.format('%dx%d=%-2d ', j, i, i * j))
        end
        print()
    end
end
function perform()
    print('========== perform ==========')
    local start = os.clock()
    for i = 0, 1000000 do end
    print('loop 1000000 times: ', os.clock() - start)
end
function main() 
    -- test for comment
    testAnd() -- test for comment too
    testOr()
    printPrime(30)
    print('factorial', 10, factorial(10))
    printFeb1()
    printFeb2()
    print9x9()
    perform()
    print('15-3*(2*2+(7-2)) = ', 15-3*(2*2+(7-2)))
end
main()
