# vim:fileencoding=gbk

import sys, os, shutil, hashlib, re

def _safePrint(*args):
    try:
        print '\t'.join(args)
    except:
        print

def _makesureDirExist(fname):
    dname = os.path.dirname(fname)
    if not os.path.isdir(dname):
        os.makedirs(dname)

def _copyFile(src, dest):
    _makesureDirExist(dest)
    shutil.copy2(src, dest)

def _moveFile(src, dest):
    _makesureDirExist(dest)
    shutil.move(src, dest) 

def _getFileHash(fname):
    md5 = hashlib.md5()
    sha1 = hashlib.sha1()
    content = file(fname, 'rb').read()
    md5.update(content)
    sha1.update(content)
    return '%s_%s' % (md5.hexdigest(), sha1.hexdigest())

def _split2HashAndFname(s):
    m = re.match(r'(\w+) (.+)', s.strip())
    return m.group(1), m.group(2)

def backupTo(hashfname, backupdname):
    os.makedirs(backupdname)
    lines = file(hashfname).readlines()
    for idx, line in enumerate(lines):
        _hash, fname = _split2HashAndFname(line)
        destfname = os.path.join(backupdname, _hash)
        _safePrint('copy %.2f%%: (%s)->(%s)' % ((idx + 1) * 100.0 / len(lines), fname, destfname))
        _copyFile(fname, destfname)
    metafname = os.path.join(backupdname, 'meta.txt')
    _safePrint('copy: (%s)->(%s)' % (hashfname, metafname))
    _copyFile(hashfname, metafname)

def restoreFrom(backupdname):
    lines = file(os.path.join(backupdname, 'meta.txt')).readlines()
    for idx, line in enumerate(lines):
        _hash, fname = _split2HashAndFname(line)
        srcfname = os.path.join(backupdname, _hash)
        _safePrint('copy %.2f%%: (%s)->(%s)' % ((idx + 1) * 100.0 / len(lines), srcfname, fname))
        _copyFile(srcfname, fname)

def genHash(listfname, hashfname):
    hashf = file(hashfname, 'w')
    lines = file(listfname).readlines()
    for idx, fname in enumerate(lines):
        fname = fname.strip()
        _safePrint('hash %.2f%%: %s' % ((idx + 1) * 100.0 / len(lines), fname))
        hashf.write('%s %s\n' % (_getFileHash(fname), fname))
    hashf.close()

def moveByHash(masterHashfname, slaveHashfname):
    hash2dfname = dict()
    moveFilePairs = []
    for line in file(masterHashfname).readlines():
        _hash, dfname = _split2HashAndFname(line)
        hash2dfname[_hash] = dfname
    for line in file(slaveHashfname).readlines():
        _hash, sfname = _split2HashAndFname(line)
        dfname = hash2dfname.get(_hash, None)
        if dfname and sfname != dfname:
            moveFilePairs.append((sfname, dfname))
    for idx, filePair in enumerate(moveFilePairs):
        sfname, dfname = filePair
        _safePrint('rename %.2f%%: (%s)->(%s)' % ((idx + 1) * 100.0 / len(moveFilePairs), sfname, dfname))
        _moveFile(sfname, dfname)

if len(sys.argv) == 1:
    _safePrint('''Usage: 
        - genHash listfname hashfname
        - moveByHash masterHashfname slaveHashfname
        - backupTo hashfname backupdname
        - restoreFrom backupdname
    ''')
else:
    globals()[sys.argv[1]](*sys.argv[2:])
