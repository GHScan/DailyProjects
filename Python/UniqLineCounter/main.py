import sys, os, subprocess, hashlib

def getFileList(dirpath):
    for dpath, dnames, fnames in os.walk(dirpath):
        for fname in fnames:
            yield os.path.join(dpath, fname)

def getTextFileList(dirpath):
    for fname in getFileList(dirpath):
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

result = set()
for dirpath in sys.argv[1:]:
    result |= set(str2MD5(line) for line in catFiles(getTextFileList(dirpath)))
print len(result)
