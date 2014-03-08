# vi:fileencoding=utf-8

import sys

for i in range(int(sys.argv[1])):
    print '127.0.0.1:7788'
    print '/dev/null'
    print 2
    print 'GET ./main HTTP/1.0'
    print ''
