-- vim:fileencoding=gbk

require 'tableHooker'

t = { army = { type = 'knight'} }

t = tableHooker.hook(t)
print('#######')
t.user = {}
t.user.resource = {gold = 1, wood = 2}
t.user.resource.stone = 3
print('gold + stone -> ', t.user.resource.gold + t.user.resource.stone)
print('army.type -> ', t.army.type)

t = tableHooker.unhook(t)
print('#######')
print(t.user.resource.gold)
