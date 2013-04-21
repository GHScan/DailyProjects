

print(collectgarbage())
for i = 1, 10000 do
    a = {1, 2, 3, {}, {}}
end
print(collectgarbage())
