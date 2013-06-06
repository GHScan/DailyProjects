# vim:fileencoding=gbk

def str2bin(s):
    return ''.join(bin(ord(c))[2:].rjust(8, '0') for c in s)
def bin2str(s):
    return ''.join(chr(int(s[i*8:i*8+8], 2)) for i in range(len(s) / 8))

s = str2bin('hello world')
print(s)
s = bin2str(s)
print(s)
