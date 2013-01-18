# vim:fileencoding=gbk

import os
import re
import urllib
import threading
from Queue import Queue
import traceback

s = raw_input('查询关键字:')
keywords = s.split(' ') if s else []

s = raw_input('排除关键字:')
rej_keywords = s.split(' ') if s else []

s = raw_input('搜索页数:(范围1~20，默认是4)')
pages = int(s) if s else 4

q = Queue()
style = ''

lock = threading.Lock()
finish_pages = 0
def report_progress():
    with lock:
        global finish_pages
        finish_pages += 1
        percent = finish_pages * 100 / pages
        if finish_pages == 1: print '进度:%d%%' % percent,
        else: print '\b\b\b\b%d%%' % percent,

def check_rejections(content, rejections):
    for rej in rejections:
        if rej in content: return False
    return True

def query_thread(idx):
    query = urllib.urlencode({'wd':' '.join(keywords), "pn":idx*10})
    try:
        html = urllib.urlopen('http://www.baidu.com/s?%s' % query).read()
    except:
        traceback.print_exc()
        html = ''

    global style
    if not style:
        reg = re.compile(r'<style>.+?</style>', re.DOTALL)
        m = reg.search(html)
        if m: style = m.group(0)

    reg = re.compile(r'<table[^>]+id="\d+"[^>]+>.+?</table><br>', re.DOTALL)
    tables = [cap for cap in reg.findall(html)]

    reg = re.compile(r'<[^>]+>', re.DOTALL)
    contents = [reg.sub(' ', tab.replace('&nbsp', ' ')) for tab in tables]

    tables = [tab for tab, content in zip(
        tables, contents) if check_rejections(content, rej_keywords)]

    q.put((idx, tables))
    report_progress()

urllib.socket.setdefaulttimeout(10)
tds = [threading.Thread(target=query_thread, args=(i,)) for i in range(pages)]
for td in tds: td.start()
for td in tds: td.join()

results = ['<title>搜索:%s,not %s</title>' % (
    ' '.join(keywords), ' '.join(rej_keywords)), style]
page_tables = []
while not q.empty():
    page_tables.append(q.get())
for idx, tables in sorted(page_tables):
    results.extend(tables)

tmp_name = "%s%sscan_tmp_di35k25h12l5i.html" % (os.environ['TEMP'], os.sep)
file(tmp_name, 'w').write('\n'.join(results))
os.system('explorer %s' % tmp_name)
