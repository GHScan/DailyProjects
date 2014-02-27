# vim:fileencoding=utf-8

import urllib2, urllib
import multiprocessing.dummy as dummy
import os 

dirpath = 'd:\百度云同步盘'

def write2file(path, content):
    newpath = ('___' + path[len(dirpath):]).decode('utf-8')
    dirs, fname = os.path.split(newpath)
    if not os.path.isdir(dirs):
        os.makedirs(dirs)
    file(newpath, 'wb').write(content)

def handle(path):
    query = urllib.urlencode({'type':'md5','path':path})
    md5 = urllib2.urlopen('http://localhost:8080/hash?' + query).read()

    _, fname = os.path.split(path)
    query = urllib.urlencode({'path':path})
    content = urllib2.urlopen('http://localhost:8080/download?' + query).read()
    write2file(path, content)

    print 'succ -> %s, %s' % (md5, path.decode('utf-8'))

pathlist = []
for line in urllib2.urlopen('http://localhost:8080/listdir?path=' + dirpath).readlines():
    pathlist.append(line.strip())

pool = dummy.Pool(16)
pool.map(handle, pathlist)
