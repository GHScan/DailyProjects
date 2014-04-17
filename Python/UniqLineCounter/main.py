import sys, os, subprocess, hashlib

def getFileNames(path):
    if os.path.isdir(path):
        for dpath, dnames, fnames in os.walk(path):
            for fname in fnames:
                yield os.path.join(dpath, fname)
    else:
        yield path

def filterTextFileNames(fnames):
    for fname in fnames:
        if  subprocess.check_output(['file', fname]).find('text') != -1:
            yield fname

def catFiles(fnames):
    for fname in fnames:
        for line in file(fname):
            yield line

def str2MD5(s):
    m = hashlib.md5()
    m.update(s)
    return m.hexdigest()

if __name__ != '__main__': exit(1)

result = set()
if len(sys.argv) == 1:
    result |= set(str2MD5(line) for line in sys.stdin.readlines())
else:
    for path in sys.argv[1:]:
        result |= set(str2MD5(line) for line in catFiles(filterTextFileNames(getFileNames(path))))
print len(result)
