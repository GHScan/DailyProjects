# vim:fileencoding=gbk

import os

path = raw_input('�����ļ���:')
assert path, '�ļ��в���Ϊ��'
if path.endswith(os.sep):
    path = path[:-1]
    assert path
print '�ļ���: ', path

posts = raw_input('����Ҫ������ļ���׺����;�ָ�:(Ĭ��.h��.cpp)')
posts = posts if posts else '.h;.cpp'
posts = posts.split(';')
assert posts, 'û�к�׺'
print '��׺: ', posts

header = raw_input('�����ļ�ͷ:(Ĭ��"// vim: fileencoding=gbk")')
header = header if header else '// vim: fileencoding=gbk'
assert header
print '�ļ�ͷ: ', header
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
