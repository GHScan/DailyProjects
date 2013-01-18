# vim:fileencoding=gbk

import os, re

def norm_dir(path):
    path = os.path.normpath(path)
    if not path.endswith(os.path.sep):
        path += os.path.sep
    return path

def move(old, new):
    print old, '->', new
    os.rename(old, new)

path = raw_input('input the path:')
assert path
path = norm_dir(path)

src_pattern = raw_input('input he source pattern:')
assert(src_pattern)
replace_pattern = raw_input('input the replace pattern:')
assert(replace_pattern)

is_recursive = raw_input('is recursive ? (y/n)').lower() == 'y'

if is_recursive:
    files = []
    for spath, _, fnames in os.walk(path):
        files.extend(spath + fname for fname in fnames)
else:
    files = [path + f for f in os.listdir(path) if os.path.isfile(path + f)]

replace_all = False
for fpath in files:
    head, tail = os.path.split(fpath)
    fname, ext = os.path.splitext(tail)
    if not re.match(src_pattern, fname):
        continue
    fname = re.sub(src_pattern, replace_pattern, fname)
    new_fpath = os.path.join(head, fname + ext)
    if replace_all:
        move(fpath, new_fpath)
    else:
        sel = raw_input(fpath + '->' + new_fpath + ' ? (y/n/a)').lower()
        if sel == 'y':
            move(fpath, new_fpath)
        elif sel == 'a':
            replace_all = True
            move(fpath, new_fpath)
        else:
            pass
