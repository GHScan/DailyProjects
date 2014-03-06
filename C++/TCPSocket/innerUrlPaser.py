# vi:fileencoding=utf-8

import sys, re, urllib2

pageUrl = sys.argv[1]
if not pageUrl.endswith('/'): pageUrl += '/'

html = urllib2.urlopen(pageUrl).read()

urls = set()
for url in re.findall(r'\bsrc=([^ ><]+)', html):
    if re.search(r'[\(\)\[\]\{\};#\?\+]', url): continue
    if re.search(r'[\x80-\xff]', url): continue
    if '\'' in url: url = re.search(r"'([^']*)'", url).group(1)
    if '"' in url: url = re.search(r'"([^"]*)"', url).group(1)
    if url.startswith('https'): continue
    if not url: continue
    if not url.startswith('http'):
        if url.startswith('/'): url = url[1:]
        url = pageUrl + url
    urls.add(url)

for url in urls: print url
