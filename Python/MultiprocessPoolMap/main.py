# vim:fileencoding=utf-8

import re, urllib2
import multiprocessing.dummy as dummy

charset_re = re.compile(r'charset=([\w-]+)')
title_re = re.compile(r'<title>([^<]+)</title>')

def captitle(url):
    req = urllib2.Request(url)
    req.add_header('User-Agent', 'Mozilla/4.0 (compatible; MSIE 5.5; Windows NT)')
    f = urllib2.urlopen(req)
    content = f.read()
    title = (['unkown'] + title_re.findall(content))[-1]
    enc = ''
    enc = enc or ([''] + charset_re.findall(f.headers.dict['content-type']))[-1]
    enc = enc or ([''] + charset_re.findall(content))[-1]
    enc = enc or 'utf-8'
    return url, title.decode(enc)

pool = dummy.Pool(16)
urls = [url.strip() for url in file('1.txt').readlines()]
for url, title in pool.map(captitle, urls):
    print '%s : %s' % (url, title)
