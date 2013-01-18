# vim:fileencoding=gbk

import os

def get_dependents(bin_path):
    dumpbin_path = os.sep.join(
            os.environ['VS80COMNTOOLS'].split(os.sep)[:2] + [
                'VC', 'bin', 'Dumpbin', 'dumpbin.exe'])

    import subprocess
    output = subprocess.check_output(
            [dumpbin_path, bin_path, '/dependents'])
    lines = (i.strip() for i in output.splitlines() if i.strip())

    from itertools import takewhile, dropwhile
    return takewhile(
            lambda a: a.endswith('.dll'),
            dropwhile(
                lambda a:not a.endswith('.dll'), lines))

dest_dir = raw_input('输入执行文件目录:')
assert dest_dir
src_dir = raw_input('输入DLL提供目录:')
assert src_dir

src_dlls = set(i for i in os.listdir(src_dir) if i.endswith('.dll'))

dest_dlls = set()
for i in os.listdir(dest_dir):
    if not i.endswith('.exe') and not i.endswith('.dll'):
        continue
    dest_dlls.update(get_dependents(dest_dir + os.sep + i))

for i in src_dlls & dest_dlls:
    src_file = src_dir + os.sep + i
    dest_file = dest_dir + os.sep + i
    if os.path.isfile(dest_file): 
        continue
    import shutil
    shutil.copyfile(src_file, dest_file)
