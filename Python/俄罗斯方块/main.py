# vim:fileencoding=gbk

if __name__ != '__main__': exit()

import wx
import random

#----------------------------------------
def posStr2posArray(s):
    r = []
    for y, l in enumerate(s.splitlines()):
        for x, c in enumerate(l):
            if c != ' ':
                r.append((x, y))
    return r

class Shape(object):
    def __init__(self, strs):
        self.posArrayIdx = 0
        self.posArrays = [posStr2posArray(s) for s in strs]
    def rotate(self):
        self.posArrayIdx = (self.posArrayIdx + 1) % len(self.posArrays)
    def rotateBack(self):
        self.posArrayIdx = (self.posArrayIdx - 1 + len(self.posArrays)) % len(self.posArrays)
    def iterPos(self, pos):
        for x, y in self.posArrays[self.posArrayIdx]:
            yield pos[0] + x, pos[1] + y

shapes = [
Shape([
'####',
'''\
#
#
#
#''',]),
Shape([
'''\
#
###''',
'''\
##
#
#''',
'''\
###
  #
''',
'''\
 #
 #
##''',]),
Shape([
'''\
  #
###''',
'''\
##
 #
 #''',
'''\
###
#  
''',
'''\
# 
# 
##''',]),
Shape([
'''\
##
##''',]),
Shape([
'''\
 ##
## ''',
'''\
# 
##
 #''',]),
Shape([
'''\
## 
 ##''',
'''\
 #
##
# ''',]),
Shape([
'''\
 #
###''',
'''\
# 
##
#''',
'''\
###
 # 
''',
'''\
 #
##
 #''',]),
        ]

#----------------------------------------
class Logic(object):
    def __init__(self, w, h):
        self.grids = [[0] * w for y in range(h)]
        self.gameOver = False
        self._newShape()
    def updateGame(self):
        assert not self.gameOver
        r = 0
        newPos = self.pos[0], self.pos[1] + 1
        if self._isShapeCollide(newPos):
            if self.pos[1] == 0:
                self.gameOver = True
            else:
                for x, y in self.shape.iterPos(self.pos):
                    self.grids[y][x] = self.rid + 1
                r = self._tryRemoveLines()
                self._newShape()
        else:
            self.pos = newPos
        return r
    def move(self, x, y):
        assert y >= 0
        newPos = self.pos[0] + x, self.pos[1] + y
        if not self._isShapeCollide(newPos):
            self.pos = newPos
    def rotate(self):
        self.shape.rotate()
        if self._isShapeCollide(self.pos):
            self.shape.rotateBack()
    def isGameOver(self):
        return self.gameOver
    def iterGrids(self):
        shapePos = list(self.shape.iterPos(self.pos))
        w, h = self._wh()
        for y in range(h):
            for x in range(w):
                v = self.grids[y][x]
                if not v and (x, y) in shapePos:
                    v = self.rid + 1
                yield v
    def _newShape(self):
        self.pos = (len(self.grids[0]) / 2, 0)
        self.rid = random.randint(0, len(shapes) - 1)
        self.shape = shapes[self.rid]
    def _isShapeCollide(self, pos):
        w, h = self._wh()
        for x, y in self.shape.iterPos(pos):
            if x < 0 or x >= w or y < 0 or y >= h: return True
            if self.grids[y][x]: return True
        return False
    def _tryRemoveLines(self):
        grids = [l for l in self.grids if not all(l)]
        r = len(self.grids) - len(grids)
        self.grids = [[0] * self._wh()[0] for i in range(r)] + grids
        return r
    def _wh(self):
        return len(self.grids[0]), len(self.grids)

#----------------------------------------
app = wx.App(False)
f = wx.Frame(None)
f.ClientSize = (300, 600)
gridW, gridH = 10, 20
keyMap = {}
brushList = [ wx.Brush((0, 255, 255)),
        wx.Brush((0, 0, 255)),
        wx.Brush((255, 128, 0)),
        wx.Brush((255, 255, 0)),
        wx.Brush((0, 255, 64)),
        wx.Brush((128, 0, 128)),
        wx.Brush((255, 0, 0)), ]
kDrive = False

def applyGameSpeed():
    global timer
    timer = wx.Timer(f)
    f.Bind(wx.EVT_TIMER, onUpdateTimer, timer)
    timer.Start(500 / level)

def startGame():
    global logic, score, level
    logic = Logic(gridW, gridH)
    score, level = 0, 1
    applyGameSpeed()

def onKeyDown(e):
    global kDrive
    if e.KeyCode in keyMap: return
    keyMap[e.KeyCode] = 1
    onKeyTimer(None)
    kDrive = True
def onKeyUp(e):
    del keyMap[e.KeyCode]
def onPaint(e):
    dc = wx.BufferedPaintDC(f)
    rt = f.GetClientRect()
    dc.DrawRectangle(*rt)
    dc.pen = wx.Pen((0, 0, 0))
    sw, sh = rt.Width / gridW, rt.Height / gridH
    gridsVal = logic.iterGrids()
    for y in range(gridH):
        for x in range(gridW):
            val = next(gridsVal)
            srt = (sw * x, sh * y, sw, sh)
            if val:
                dc.Brush = brushList[val - 1]
                dc.DrawRectangle(*srt)
def onUpdateTimer(e):
    global score, level, logic
    score += logic.updateGame() * 10
    if score / 100 + 1 != level:
        level = score / 100 + 1
        applyGameSpeed()
    f.Title = '关卡=%d，积分=%d' % (level, score)
    f.Refresh()
    if logic.isGameOver():
        wx.MessageBox('游戏结束！')
        startGame()
def onKeyTimer(e):
    global kDrive
    if kDrive:
        kDrive = False
        return
    global logic
    if not keyMap: return
    if wx.WXK_ESCAPE in keyMap:
        exit()
    if wx.WXK_SPACE in keyMap:
        startGame()
    if wx.WXK_LEFT in keyMap:
        logic.move(-1, 0)
    if wx.WXK_RIGHT in keyMap:
        logic.move(1, 0)
    if wx.WXK_DOWN in keyMap:
        logic.move(0, 2)
    if wx.WXK_UP in keyMap:
        logic.rotate()
    f.Refresh()

logic = Logic(gridW, gridH)
f.Bind(wx.EVT_KEY_DOWN, onKeyDown)
f.Bind(wx.EVT_KEY_UP, onKeyUp)
f.Bind(wx.EVT_PAINT, onPaint)
f.Bind(wx.EVT_ERASE_BACKGROUND, lambda e:e)
kTimer = wx.Timer(f)
f.Bind(wx.EVT_TIMER, onKeyTimer, kTimer)
kTimer.Start(100)
startGame()

f.CenterOnScreen()
f.Show()
app.MainLoop()
