# vim:fileencoding=gbk

import os
from itertools import groupby

path = raw_input('输入漫画路径:')
assert path, '必须输入漫画路径'

s = raw_input('输入每页的图片数:')
page_img_cnt = int(s) if s else 32

os.system('rm -rf comics')
os.mkdir('comics')
os.chdir('comics')

img_names = []
for dir_name, dirs, files in os.walk(path):
    img_names.extend(os.path.join(dir_name, fname) for fname in files)

page_header = '''
<hr>
<a href="%d.html">上一页</a>
<a href="%d.html">下一页</a>
<hr>
'''
page_item = '''
<img src="%s"/>
<hr>
'''

for page_id, fnames in groupby(
        enumerate(img_names), lambda a:a[0] /page_img_cnt):
    f = file('%d.html' % page_id, 'w')
    f.write(page_header % (page_id - 1, page_id + 1))
    for fname in fnames:
        f.write(page_item % fname[1])
    f.write(page_header % (page_id - 1, page_id + 1))
    f.close()
