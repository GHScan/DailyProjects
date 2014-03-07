#! vim:fileencoding=utf-8

import sys

for i in range(int(sys.argv[1])):
    print 'host: hhcomic.com:80'
    print 'output: tmp/%s.txt' % i
    print 'request: 3'
    print 'GET http://hhcomic.com/ HTTP/1.0'
    print 'User-Agent: Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/33.0.1750.117 Safari/537.36'
    print ''
