# vim:fileencoding=gbk

import wx

app = wx.App(False)
frame = wx.Frame(None, wx.ID_ANY, 'by Scan')

def OnPaint(evt):
    pdc = wx.PaintDC(frame)
    try:
        dc = wx.GCDC(pdc)
    except:
        dc = pdc

    for i in range(0, 200):
        dc.DrawPoint(100, i)

def OnKeyUp(evt):
    if evt.KeyCode == 27:
        exit()

frame.Bind(wx.EVT_PAINT, OnPaint)
frame.Bind(wx.EVT_KEY_UP, OnKeyUp)
frame.Center()
frame.Show()
app.MainLoop()
