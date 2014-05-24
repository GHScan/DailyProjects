# 1. all function is generator, with a extra arg as coroutine self, you can use it for resume
# 2. every callsite should be: for _ in f(...): yield _
# 3. the real yield is: yield True

import random, math
#------------------------------
class Coroutine(object):
    def __init__(self, id, f, *args):
        self._topGen = f(self, *args)
        self._id = id
        self.resume()
    def resume(self):
        self._alive = next(self._topGen, False)
    def alive(self):
        return self._alive
    def id(self):
        return self._id
#------------------------------
_taskList = []
_events = []
_now = 0
def time():
    return _now
def insertEvent(delay, callback):
    finish = time() + delay
    i = 0
    while i < len(_events) and _events[i][0] <= finish:
        i += 1
    _events.insert(i, [finish, callback])
def dispatchEvent():
    if len(_events):
        e = _events.pop(0)
        global _now
        _now = e[0]
        e[1]()
        return True
    else:
        return False
#------------------------------
def connectToServerAndCheck(co, nums):
    def _callback():
        c = math.sqrt(reduce(lambda a,b:a+b, (i * i for i in nums), 0))
        if c == int(c):
            print '[%d] co_%d: %s' % (time(), co.id(), nums)
        co.resume()
    insertEvent(random.randint(1, 10), _callback)
    yield True
def thread(co, first, last, depth, nums):
    if depth == 0:
        for _ in connectToServerAndCheck(co, nums): yield _
    else:
        for i in range(first, last):
            newNums = nums + (i,)
            for _ in thread(co, i, last, depth - 1, newNums): yield _
#------------------------------
for i in range(5):
    Coroutine(i, thread, i*30+1, (i+1)*30, 2, tuple())
while dispatchEvent(): pass
