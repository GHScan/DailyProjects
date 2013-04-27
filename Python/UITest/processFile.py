# vim:fileencoding=gbk

import wx
import os

def processFile(fpath):
    print fpath

class FileDropTarget(wx.FileDropTarget):
    def __init__(self, window):
        wx.FileDropTarget.__init__(self)
        window.SetDropTarget(self)
    def OnDropFiles(self, x, y, paths):
        for f in self.getFiles(paths):
            processFile(f)
    def getFiles(self, paths):
        for path in paths:
            if os.path.isdir(path):
               files = self.getFiles(os.path.join(path, fname) for fname in os.listdir(path))
               for f in files: yield f
            else:
                yield path

def OnKeyUp(evt):
    if evt.KeyCode == 27:
        exit()

app = wx.App(False)
frame = wx.Frame(None, wx.ID_ANY, 'by Scan')
FileDropTarget(frame)
frame.Bind(wx.EVT_KEY_UP, OnKeyUp)
frame.Center()
frame.Show()
app.MainLoop()
