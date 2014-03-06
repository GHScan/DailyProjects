#! vim:fileencoding=utf-8

import sys, urllib2

existPaths = set()

for i, url in enumerate(sys.stdin.readlines()):
    url = url.strip()
    urlObj = urllib2.urlparse.urlparse(url)

    path = urllib2.posixpath.split(url)[1]
    if not path or path in existPaths:
        path = '%s%s' % (i, urllib2.posixpath.splitext(url)[1] or '.txt')
    existPaths.add(path)

    print urlObj.netloc
    print 'tmp/%s' % path
    print '3'
    print 'GET %s HTTP/1.0' % url
    print 'User-Agent: Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/33.0.1750.117 Safari/537.36'
    print ''
