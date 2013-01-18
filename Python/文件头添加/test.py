# vim:fileencoding=gbk

import os

path = raw_input('输入文件夹:')
assert path, '文件夹不能为空'
if path.endswith(os.sep):
    path = path[:-1]
    assert path
print '文件夹: ', path

posts = raw_input('输入要处理的文件后缀，用;分隔:(默认.h、.cpp)')
posts = posts if posts else '.h;.cpp'
posts = posts.split(';')
assert posts, '没有后缀'
print '后缀: ', posts

header = raw_input('输入文件头:(默认"// vim: fileencoding=gbk")')
header = header if header else '// vim: fileencoding=gbk'
assert header
print '文件头: ', header
header += '\n'

for fname in os.listdir(path):
    full_name = path + os.sep + fname
    if (not os.path.isfile(full_name)) or (
            os.path.splitext(fname)[1] not in posts):
        continue
    data = file(full_name).read()
    if data.startswith(header):
        print 'skip file : ', fname
        continue
    print 'process file : ', fname
    f = file(full_name, 'w')
    f.write(header)
    f.write(data)
    f.close()
