# vim:fileencoding=gbk

import urllib
import binascii
import os

start_text = 'var qTcms_S_m_murl_e="'
for line in urllib.urlopen(raw_input('输入某话第一页地址:')):
    if line.startswith(start_text):
        target_text = line[len(start_text):-1]
        break

file_name = os.environ['TEMP'] + os.sep + 'scan_3jb9caz82jf.html'

with file(file_name, 'w') as f:
    for url in (j for j in binascii.a2b_base64(
        target_text).split('$') if j.startswith('http')):
        f.write('<img src="%s"/>\n' % url)

os.system('explorer ' + file_name)
