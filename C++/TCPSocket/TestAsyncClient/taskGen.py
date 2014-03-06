# vi:fileencoding=utf-8

import sys

for i in range(int(sys.argv[1])):
    print 'www.baidu.com'
    print 'tmp/%d.txt' % i
    print 2
    print 'GET / HTTP/1.0'
    print ''
