-- vim:fileencoding=gbk

require 'GameGlobalData'
require 'LuaStringUtils'
require 'LuaTableUtils'

local dataChangeTimes = {
    ['user'] = 0,
    ['user.resource'] = 0,
    ['user.armies'] = 0,
    ['config'] = 0,
}
local listeners = {}
-- add listeners
for k in pairs(dataChangeTimes) do
    local listener = function()
        dataChangeTimes[k] = dataChangeTimes[k] + 1
    end
    listeners[k] = listener
    GameGlobalData:AddListener(k, listener)
end

-- test1
GameGlobalData:Merge('user', {resource = {}, armies = {}})
GameGlobalData:ConsumeActiveListeners()
assert(tableext.equal(dataChangeTimes, {
    ['user'] = 1,
    ['user.resource'] = 1,
    ['user.armies'] = 1,
    ['config'] = 0,
}))

-- test2
GameGlobalData:Merge('user.resource', { gold = 10, stone = 5})
GameGlobalData:ConsumeActiveListeners()
assert(tableext.equal(dataChangeTimes, {
    ['user'] = 2,
    ['user.resource'] = 2,
    ['user.armies'] = 1,
    ['config'] = 0,
}))

-- test3
GameGlobalData:Merge('user.resource', { gold = 10, stone = 5})
GameGlobalData:ConsumeActiveListeners()
assert(tableext.equal(dataChangeTimes, {
    ['user'] = 2,
    ['user.resource'] = 2,
    ['user.armies'] = 1,
    ['config'] = 0,
}))

-- test4
GameGlobalData:Merge('user.resource', { gold = 11})
GameGlobalData:ConsumeActiveListeners()
assert(tableext.equal(dataChangeTimes, {
    ['user'] = 3,
    ['user.resource'] = 3,
    ['user.armies'] = 1,
    ['config'] = 0,
}))

-- test5
GameGlobalData:Merge('user.armies', {{uid = 2, type = 'archer'}, { uid = 1, type = 'knight'}})
GameGlobalData:ConsumeActiveListeners()
assert(tableext.equal(dataChangeTimes, {
    ['user'] = 4,
    ['user.resource'] = 3,
    ['user.armies'] = 2,
    ['config'] = 0,
}))

-- test6
GameGlobalData:Merge('user.armies', {{ uid = 1, type = 'knight'}, {uid = 2, type = 'archer'}, })
GameGlobalData:ConsumeActiveListeners()
assert(tableext.equal(dataChangeTimes, {
    ['user'] = 4,
    ['user.resource'] = 3,
    ['user.armies'] = 2,
    ['config'] = 0,
}))

-- test7
GameGlobalData:Merge('user.armies', {{ uid = 1, type = 'knight2'},  })
GameGlobalData:ConsumeActiveListeners()
assert(tableext.equal(dataChangeTimes, {
    ['user'] = 5,
    ['user.resource'] = 3,
    ['user.armies'] = 3,
    ['config'] = 0,
}))
assert(GameGlobalData:GetData('user.armies.1').type == 'knight2')

-- test8
GameGlobalData:Delete('user.resource')
assert(tableext.equal(dataChangeTimes, {
    ['user'] = 5,
    ['user.resource'] = 3,
    ['user.armies'] = 3,
    ['config'] = 0,
}))
GameGlobalData:ConsumeActiveListeners()
assert(tableext.equal(dataChangeTimes, {
    ['user'] = 6,
    ['user.resource'] = 4,
    ['user.armies'] = 3,
    ['config'] = 0,
}))
assert(not GameGlobalData:GetData('user.resource'))

-- test9
local armies = GameGlobalData:GetData('user.armies')
armies['1'].type = 'knight3'
armies['2'].type = 'archer2'
assert(tableext.equal(dataChangeTimes, {
    ['user'] = 6,
    ['user.resource'] = 4,
    ['user.armies'] = 3,
    ['config'] = 0,
}))
GameGlobalData:NotifyDataChange('user.armies')
GameGlobalData:ConsumeActiveListeners()
assert(tableext.equal(dataChangeTimes, {
    ['user'] = 7,
    ['user.resource'] = 4,
    ['user.armies'] = 4,
    ['config'] = 0,
}))

-- test10
GameGlobalData:Merge('config', {language = 'en'})
assert(tableext.equal(dataChangeTimes, {
    ['user'] = 7,
    ['user.resource'] = 4,
    ['user.armies'] = 4,
    ['config'] = 0,
}))
GameGlobalData:ConsumeActiveListeners()
assert(tableext.equal(dataChangeTimes, {
    ['user'] = 7,
    ['user.resource'] = 4,
    ['user.armies'] = 4,
    ['config'] = 1,
}))
GameGlobalData:RemoveListener('config', listeners['config'])
GameGlobalData:Merge('config', {language = 'cn'})
GameGlobalData:ConsumeActiveListeners()
assert(tableext.equal(dataChangeTimes, {
    ['user'] = 7,
    ['user.resource'] = 4,
    ['user.armies'] = 4,
    ['config'] = 1,
}))
assert(GameGlobalData:GetData('config').language == 'cn')

-- test11
GameGlobalData:Merge('config', {language = 'ru'})
GameGlobalData:Merge('user.resource', {gold = 100})
GameGlobalData:Merge('user.resource', {wood = 200})
GameGlobalData:Merge('user.armies', {{uid = 4, type = 'assassin'}})
assert(tableext.equal(dataChangeTimes, {
    ['user'] = 7,
    ['user.resource'] = 4,
    ['user.armies'] = 4,
    ['config'] = 1,
}))
GameGlobalData:ConsumeActiveListeners()
assert(tableext.equal(dataChangeTimes, {
    ['user'] = 8,
    ['user.resource'] = 5,
    ['user.armies'] = 5,
    ['config'] = 1,
}))
assert(tableext.requal(GameGlobalData:GetData('user.armies'), {
    ['1'] = {uid = 1, type='knight3'},
    ['2'] = {uid = 2, type='archer2'},
    ['4'] = {uid = 4, type='assassin'},
}))

print('GameGlobalData -> test is ok')

do
    collectgarbage()

    local N = 500
    local start = os.clock()
    local data = {
        resource = { gold = 10, wood = 12, food = 15}, 
        armies = {
            { uid = 5, type = 'knight', amount = 10}, 
            { uid = 6, type = 'archer', amount = 10}, 
            { uid = 7, type = 'assassin', amount = 10}
        },
    }
    for i = 1, N do
        GameGlobalData:Merge('user', data)
        GameGlobalData:ConsumeActiveListeners()
        data.resource.gold = data.resource.gold + 1
        data.armies[1].amount = data.armies[1].amount + 1
    end
    globalMergeCost = os.clock() - start

    collectgarbage()

    start = os.clock()
    data.resource.gold = 10
    local dest = {}
    for i = 1, N do
        tableext.merge(dest, data)
        data.resource.gold = data.resource.gold + 1
        data.armies[1].amount = data.armies[1].amount + 1
    end
    tableMergeCost = os.clock() - start

    print('GameGlobalData performance test ->\n\tloop:', N, '\n\t', 
        'GameGlobalData:Merge:', globalMergeCost, 'tableext.merge:', tableMergeCost)
end
