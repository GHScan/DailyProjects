#! vim:fileencoding=utf-8

from __future__ import print_function
import sys, os, re
import urllib2
import multiprocessing.dummy as dummy

def safePrint(*args):
    try:
        with g_gLock:
            print(*args)
    except: pass

def getBookUrls(url):
    urlobj = urllib2.urlparse.urlparse(url)

    html = urllib2.urlopen(url).read()

    comicID = re.search(r'var ComicID=(\d+)', html).group(1)
    bookUrls = []
    for m in re.finditer(r'<a href=(/hhpage/%s/\S+) target=_blank>([^<]+)</a>' % comicID, html):
        bookname = m.group(2).decode('gb18030')
        bookurl = urllib2.urlparse.urlunparse((urlobj.scheme,urlobj.netloc,m.group(1))+('',)*3)
        bookUrls.append((bookname, bookurl))
    return sorted(bookUrls, cmp=lambda a,b:-cmp(a[0], b[0]))

def getServerList(url):
    urlobj = urllib2.urlparse.urlparse(url)
    jsUrl = urllib2.urlparse.urlunparse((urlobj.scheme,urlobj.netloc,'/h/js.js')+('',)*3)
    js = urllib2.urlopen(jsUrl).read()

    servers = ['']*64
    for m in re.finditer(r'ServerList\[(\d+)\]="([^"]+)"', js):
        server = m.group(2)
        if server.endswith('/'): server = server[:-1]
        servers[int(m.group(1))] = server
    return servers;

def decodeImageUrls(s, a='tavzscoewrm'):
    for i, c in enumerate(a[:-1]):
        s = s.replace(c, str(i))
    chars = (int(s) for s in s.split(a[-1]))
    return (''.join(chr(c) for c in chars)).split('|')

def getBookImageUrls(url, servers):
    html = urllib2.urlopen(url).read()
    serverIdx = int(re.search('s=(\d+)$', url).group(1)) - 1
    server = servers[serverIdx]

    imgUrls = decodeImageUrls(re.search(r'var PicLlstUrl = "(\w+)"', html).group(1))
    imgUrls = [server + url for url in imgUrls]
    return imgUrls

def downloadImage(url, path):
    if os.path.isfile(path): return

    req = urllib2.Request(url)
    req.add_header('User-Agent', 'Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/33.0.1750.117 Safari/537.36')
    content = urllib2.urlopen(req).read()

    dname = os.path.dirname(path)
    if dname and not os.path.isdir(dname):
        os.makedirs(dname)
    file(path, 'wb').write(content)
    safePrint(u'downloaded -> %s\n' % path)

def downloadBook(dirname, url, servers):
    imgUrls = getBookImageUrls(url, servers)
    urlPaths = []
    for imgUrl in imgUrls:
        path = os.path.join(dirname, urllib2.posixpath.basename(imgUrl).decode('gb18030'))
        urlPaths.append((imgUrl, path))

    safePrint(u'********** Start download: %s, (%d images)\n' % (dirname, len(urlPaths)))
    g_pool.map(lambda t:downloadImage(*t), urlPaths)
    safePrint(u'********** Finish download: %s, (%d images)\n' % (dirname, len(urlPaths)))

def downloadBooks(url):
    safePrint(u'downloading the book list...')
    servers = getServerList(url)
    bookUrls = getBookUrls(url)
    safePrint(u'found %d books !\n' % len(bookUrls))
    for bookName, bookUrl in filterBooksByUser(bookUrls):
        downloadBook(os.path.join(u'Books', bookName), bookUrl, servers)

def filterBooksByUser(bookUrls):
    for name, url in bookUrls: safePrint(name + u'\n')

    while True:
        keyword = raw_input(u'input keyword(regex) to filter books:\n').decode(sys.getfilesystemencoding())
        names = [name for name, url in bookUrls if re.search(keyword, name)]
        for name in names: safePrint(name + u'\n')
        if raw_input(u'is the book list ok?(y/n):\n') == 'y': break

    book2Url = dict(bookUrls)
    return [(name, book2Url[name]) for name in names]


if __name__ != '__main__': exit(0)
if len(sys.argv) < 2:
    print('Usage : python %s url' % sys.argv[0])
    exit(0)

g_gLock = dummy.Lock()
g_pool = dummy.Pool(16)
downloadBooks(sys.argv[1])
