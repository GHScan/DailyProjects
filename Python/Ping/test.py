# vim:fileencoding=gbk

import threading
import subprocess
import re

urls = [
"www.baidu.com", "www.qq.com", "www.sina.com", "www.sohu.com",
"www.taobao.com", "www.163.com", "www.tianya.cn", "www.mop.com",
]

lock = threading.Lock()

def ping_thread(url):
    try:
        output = subprocess.check_output('ping %s' % url)
    except:
        output = ''
    if output.find('Pinging') >= 0:
        reg = re.compile(r' time=(\d+)ms TTL')
    else:
        reg = re.compile(r' 时间=(\d+)ms TTL')
    avrMs = resCnt = 0
    for cap in reg.findall(output):
        resCnt += 1
        avrMs += int(cap)
    if resCnt: avrMs /= resCnt
    with lock:
        print url.ljust(30), ' : ', '%d(%d/4)' % (avrMs, resCnt)

tds = [threading.Thread(target=ping_thread, args=(url,)) for url in urls]
for td in tds: td.start()
for td in tds: td.join()

raw_input('按任意键继续...')
