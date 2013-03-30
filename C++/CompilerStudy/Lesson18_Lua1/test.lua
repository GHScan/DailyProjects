
local t = os.clock()
for i = 1, 1000000 do end
print(os.clock() - t)

t = os.clock()
local i = 1
while i < 1000000 do i = i + 1 end
print(os.clock() - t)
