# vim:fileencoding=gbk

import os
import wx

app = wx.App(False)
f = wx.Frame(None, wx.ID_ANY, '脚本调度')
f.ClientSize = (200, 400)

def onScriptSelected(sname):
    os.system('cls')
    cmd = 'python _%s.py' % sname.encode('gbk')
    print 'command : ', cmd, '\n\n'
    os.system(cmd)

def onKeyUp(e):
    if e.KeyCode == wx.WXK_ESCAPE:
        exit()

l = wx.ListBox(f, wx.ID_ANY)
for i in os.listdir('.'):
    if i.startswith('_') and os.path.splitext(i)[1] in ['.py']:
        l.Insert(i[1:-3], l.Count)

l.Bind(wx.EVT_LISTBOX_DCLICK, lambda e: onScriptSelected(e.String))
l.Bind(wx.EVT_KEY_UP, lambda e: onKeyUp(e))

f.Show()
f.CenterOnScreen()
app.MainLoop()
