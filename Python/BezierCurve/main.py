#! /usr/bin/env python
# coding=utf-8

import wx

def A_n(n, i):
    return reduce(lambda a, b: a * b, range(n - i + 1, n + 1), 1)
def C_ni(n, i):
    return A_n(n, i) / A_n(i, i)

def bezier(points, t):
    opt = 1 - t
    fac1 = opt ** (len(points) - 1)
    x, y = 0, 0
    for i, pt in enumerate(points):
        fac2 = C_ni(len(points) - 1, i)
        fac = fac1 * fac2
        x += fac * pt[0]
        y += fac * pt[1]
        fac1 *= t / opt
    return x, y
#====================

app = wx.App(False)
f = wx.Frame(None, wx.ID_ANY, 'bezier')

def OnPaint(evt):
    pdc = wx.PaintDC(f)
    try:
        dc = wx.GCDC(pdc)
    except:
        dc = pdc

    # 2 order
    #points = [(100, 0), (150, 100), (100, 200)]
    # 3 order
    #points = [(100, 0), (300, 0), (300, 200), (100, 200)]
    # 4 order
    points = [(50, 200), (40, 0), (240, 0), (270, 200), (320, 30)]
    lastpt = points[0]
    for i in range(0, 200):
        newpt = bezier(points, i / 200.0)
        dc.DrawLinePoint(lastpt, newpt)
        lastpt = newpt

def OnKeyUp(evt):
    if evt.KeyCode == 27:
        exit()

f.Bind(wx.EVT_PAINT, OnPaint)
f.Bind(wx.EVT_KEY_UP, OnKeyUp)
f.Show()
f.Center()
app.MainLoop()
