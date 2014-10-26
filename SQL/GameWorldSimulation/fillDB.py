import time
import random
import sys
import datetime
import getpass
#------------------------------
def randomChoose(a, e):
    r = random.random()
    return a[int(len(a) * (r ** e))]

class Timer(object):
    def __init__(self, name):
        self.mName = name
    def __enter__(self):
        self.mStart = time.clock()
    def __exit__(self, *args):
        print '%s: %.3fs' % (self.mName, time.clock() - self.mStart)
#------------------------------
kFirstNames = ['James', 'John', 'Robert', 'Michael', 'William', 'David', 'Richard', 'Charles', 'Joseph', 'Thomas', 'Christopher', 'Daniel', 'Paul', 'Mark', 'Donald', 'George', 'Kenneth', ]
kLastNames = ['Smith', 'Johnson', 'Williams', 'Jones', 'Brown', 'Davis', 'Miller', 'Wilson', 'Moore', 'Taylor', 'Anderson', 'Thomas', 'Jackson', 'White', 'Harris', 'Martin', 'Thompson', 'Garcia', 'Martinez', 'Robinson', 'Clark', 'Rodriguez', 'Lewis', ]

gPlayerNames = set()
def genPlayerName():
    name = '%s %s' % (random.choice(kFirstNames), random.choice(kLastNames))
    i = 1
    while name in gPlayerNames:
        name = name + str(i)
        i += 1
    gPlayerNames.add(name)
    return name

gLastGangNameID = 0
def genGangName():
    global gLastGangNameID
    name = '[Gang] %s %s' % (random.choice(kLastNames), gLastGangNameID)
    gLastGangNameID += 1
    return name
#------------------------------
kMaxGangLevel = 15
kMaxPlayerLevel = 40
kMaxPlayerGold = 100000
kMaxPlayerGem = 1000
kMaxItemID = 120
kMaxItemExp = 250

gGangNum = None
gGangs = None
def genGangs():
    global gGangs
    gangLevels = range(1, kMaxGangLevel)
    gGangs = []
    for i in range(gGangNum):
        level = randomChoose(gangLevels, 3)
        gGangs.append((i + 1, genGangName(), 'Gang desc %s' % i, level))
    gGangs.sort(key=lambda g:g[3], reverse=True)

gPlayerNum = None
gPlayers = None
def genPlayers():
    global gPlayers
    playerLevels = range(1, kMaxPlayerLevel)
    playerGolds = range(1, kMaxPlayerGold)
    playerGems = range(1, kMaxPlayerGem)
    gPlayers = []
    for i in range(gPlayerNum):
        level = randomChoose(playerLevels, 2)
        gangID = randomChoose(gGangs, 2)[0]
        login = datetime.datetime.now() - datetime.timedelta(days=random.randrange(0, 365))
        gem = randomChoose(playerGems, 1 + 3 * (1 - (level / kMaxPlayerLevel)))
        gold = randomChoose(playerGolds, 1 + 2 * (1 - level / kMaxPlayerLevel) + 2 * (1 - gem / kMaxPlayerGem))
        gPlayers.append((i + 1, genPlayerName(), gangID, login, level, gold, gem))
    gPlayers.sort(key=lambda p:p[4], reverse=True)

gItemNum = None
gItems = None
def genItems():
    global gItems
    itemIDs = range(1, kMaxItemID)
    itemExps = range(1, kMaxItemExp)
    gItems = []
    for i in range(gItemNum):
        player = randomChoose(gPlayers, 2)
        playerID = player[0]
        playerLevel = player[4]
        playerGold = player[5]
        playerGem = player[6]
        itemID = randomChoose(itemIDs, 1 + (1 - playerLevel / kMaxPlayerLevel) + 3 * (1 - playerGem / kMaxPlayerGem))
        exp = randomChoose(itemExps, 1 + (1 - playerGold / kMaxPlayerGold) + 2 * (1 - playerGem / kMaxPlayerGem))
        gItems.append((i + 1, itemID, playerID, exp))
    gItems.sort(key=lambda p:p[1], reverse=True)

#------------------------------
def batchInsert(conn, sql, data, batchSize):
    cursor = conn.cursor()
    i = 0
    while i < len(data):
        cursor.executemany(sql, data[i:min(len(data), i + batchSize)])
        conn.commit()
        i += batchSize

def fillMySQL(*args, **dargs):
    import mysql.connector 

    conn = mysql.connector.connect(*args, **dargs)

    # make sure the mysqld variable 'max_allowed_packet' is large enough!
    kBatchSize = 10000
    batchInsert(conn, 'insert into gangs(id,name,description,level) values(%s,%s,%s,%s)', gGangs, kBatchSize)
    batchInsert(conn, 'insert into players(id,name,gang_id,last_login,level,gold,gem) values(%s,%s,%s,%s,%s,%s,%s)', gPlayers, kBatchSize)
    batchInsert(conn, 'insert into items(id,item_id,player_id,exp) values(%s,%s,%s,%s)', gItems, kBatchSize)

    conn.close()

def fillSqlite(*args, **dargs):
    import sqlite3

    conn = sqlite3.connect(*args, **dargs)
    cursor = conn.cursor()

    cursor.executemany('insert into gangs(id,name,description,level) values(?,?,?,?)', gGangs)
    cursor.executemany('insert into players(id,name,gang_id,last_login,level,gold,gem) values(?,?,?,?,?,?,?)', gPlayers)
    cursor.executemany('insert into items(id,item_id,player_id,exp) values(?,?,?,?)', gItems)

    conn.commit()
#------------------------------
gGangNum = 200 * 10
gPlayerNum = 10000 * 10
gItemNum = 80000 * 10

with Timer('genGang'): genGangs()
with Timer('genPlayers'): genPlayers()
with Timer('genItems'): genItems()

with Timer('fillDB'):
    fillMySQL(user=raw_input('user:'),password=getpass.getpass(),host=raw_input('host:'),database=raw_input('db:'))
    #fillSqlite(raw_input('db path:'))

print 'success!'
