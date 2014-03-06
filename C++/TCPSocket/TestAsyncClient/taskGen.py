# vi:fileencoding=utf-8

import sys

for i in range(int(sys.argv[1])):
    print 'www.hhcomic.com'
    print 'tmp/%d.txt' % i
    print 2
    print 'GET http://www.hhcomic.com HTTP/1.0'
    print ''
