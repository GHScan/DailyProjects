-- vim:fileencoding=gbk

require 'tableHooker'


a = {}
a.user = {}
a.user.army = { type = 'knight' }

a = tableHooker.hook(a)
--tableHooker.enableAutoInvokeListeners(a, false)
tableHooker.addListener(a, 'user', function()
    print('dataChanged: user')
end)
tableHooker.addListener(a, 'user.army', function() 
    print('dataChanged: user.army') 
end)
tableHooker.addListener(a, 'user.resource', function() 
    print('dataChanged: user.resource') 
end)
tableHooker.addListener(a, 'user.resource.stone', function()
    print('dataChanged: user.resource.stone')
end)

print('##############')
a.user.army.type = 'archer'
print('##############')
a.user.army.type = 'archer'
print('##############')
a.user.army.amount = 10
print('##############')
a.user.resource = {gold = 5}
print('##############')
a.user.resource.stone = 7
print('##############')
a.user.resource.gold = 8
--tableHooker.invokeActiveListeners(a)
print('##############')
for k, v in tableHooker._pairs(a.user.resource) do
    print(k, v)
end
print('##############')

a = tableHooker.unhook(a)

print(a.user.army.type)
