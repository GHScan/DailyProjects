# vim:fileencoding=gbk

import struct

def writeChunk(f, n, c):
    f.write(struct.pack('ic', n, c))
def readChunk(f):
    s = f.read(struct.calcsize('ic'))
    if not s : return None, None
    return struct.unpack('ic', s)

def compress(fsrc, fdest):
    t = dict(zip((chr(i) for i in range(256)), (i for i in range(256))))
    nid = 256
    s = ''
    for c in fsrc.read():
        s += c
        if s not in t:
            t[s] = nid
            nid += 1
            writeChunk(fdest, t[s[:-1]], s[-1])
            s = ''
    if s: writeChunk(fdest, t[s], ' ') # 注意多输出了个空格
def decompress(fsrc, fdest):
    t = dict(zip((i for i in range(256)), (chr(i) for i in range(256))))
    nid = 256
    while True:
        n, c = readChunk(fsrc)
        if n == None: break
        s = t[n] + c
        fdest.write(s)
        t[nid] = s
        nid += 1

compress(file('1.txt', 'rb'), file('2.txt', 'wb'))
decompress(file('2.txt', 'rb'), file('3.txt', 'wb'))
