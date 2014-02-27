# vim:fileencoding=utf-8

from __future__ import unicode_literals
import urllib2, urllib
import multiprocessing.dummy as dummy
import os, sys

g_netHost = 'localhost:8080'
g_netDirPath = ''
g_localDirPath = ''

g_lock = dummy.Lock()
g_downloadedCount = 0
g_filePathList = []

def saveContent2File(filePath, content):
    filePath = g_localDirPath + filePath[len(g_netDirPath):]
    dirs, fname = os.path.split(filePath)
    if not os.path.isdir(dirs):
        os.makedirs(dirs)
    file(filePath, 'wb').write(content)

def downloadFile(filePath):
    _, fname = os.path.split(filePath)
    query = urllib.urlencode({b'path':filePath.encode('utf-8')}).decode('utf-8')
    url = 'http://%s/download?%s' % (g_netHost, query)
    content = urllib2.urlopen(url).read()
    saveContent2File(filePath, content)

    with g_lock:
        global g_downloadedCount
        g_downloadedCount += 1
        try:
            print 'Downloaded %.2f%% -> %s\n' % (100.0 * g_downloadedCount / len(g_filePathList), filePath)
        except Exception as e: pass

def listFilePath():
    query = urllib.urlencode({b'path':g_netDirPath.encode('utf-8')}).decode('utf-8')
    url = 'http://%s/listdir?%s' % (g_netHost, query)
    for line in urllib2.urlopen(url).readlines():
        g_filePathList.append(line.decode('utf-8').strip())

def userInput():
    global g_netHost, g_netDirPath, g_localDirPath
    i = raw_input('input net host(default=localhost:8080):').decode(sys.getfilesystemencoding())
    if i: g_netHost = i
    g_netDirPath = raw_input('input net dir:').decode(sys.getfilesystemencoding())
    assert g_netDirPath
    g_localDirPath = raw_input('input local dir:').decode(sys.getfilesystemencoding())
    if not g_localDirPath: g_localDirPath = os.path.split(g_netDirPath)[1] or 'temp'

    if not g_netDirPath.endswith(os.path.sep): g_netDirPath += os.path.sep
    if not g_localDirPath.endswith(os.path.sep): g_localDirPath += os.path.sep

userInput()
listFilePath()
dummy.Pool(16).map(downloadFile, g_filePathList)
