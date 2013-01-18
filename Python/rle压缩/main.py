# vim:fileencoding=gbk

import struct

def writeChunk(f, n, c):
    f.write(struct.pack('bc', n, c))
def readChunk(f):
    s = f.read(2)
    if not s : return None, None
    return struct.unpack('bc', s)

def compress(fsrc, fdest):
    n, lc = 1, fsrc.read(1)
    for c in fsrc.read():
        if lc == c and n < 255:
            n += 1
        else:
            writeChunk(fdest, n, lc)
            n, lc = 1, c
    if n: writeChunk(fdest, n, lc)
def decompress(fsrc, fdest):
    while True:
        n, c = readChunk(fsrc)
        if n == None: break
        fdest.write(c * n)

compress(file('1.txt', 'rb'), file('2.txt', 'wb'))
decompress(file('2.txt', 'rb'), file('3.txt', 'wb'))
