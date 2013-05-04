
print('!!!!!!!!!!!! content 1:')
for l in io.lines('test/io.lua') do
    print(l)
end

print('!!!!!!!!!!!! content 2:')
print(io.open('test/io.lua'):read('*a'))
