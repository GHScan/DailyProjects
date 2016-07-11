import urllib2, re, os, sys
from multiprocessing.pool import ThreadPool
from urlparse import parse_qs, urlparse

def _download_url(url):
    for i in xrange(sys.maxint):
        try:
            return urllib2.urlopen(url, timeout = 10).read()
        except:
            if i == 2:
                raise

def get_chapters(dir_url):
    content = _download_url(dir_url).decode('gb18030')
    start = content.index('<ul class="bi">')
    end = content.index('</ul>', start)
    content = content[start:end]

    chs = []
    for m in re.finditer('<li><a href=([^ ]+) target=_blank>([^<]+)</a>', content): 
        chs.append({'name':m.group(2), 'url': dir_url[:dir_url.index('/', len('http://'))] + m.group(1)})
    return chs

def _unsuan(s, sk):
    k=sk[:-1]
    f=sk[-1:]
    for i in range(len(k)):
        s = s.replace(k[i], str(i))
    ss = s.split(f)
    s=""
    for c in ss:
        s += chr(int(c))
    return s

def _getServerList():
    ServerList=[ "http://104.237.55.70:9393/dm01/",
    "http://64.185.235.244:9393/dm02/",
    "http://64.185.235.244:9393/dm03/",
    "http://104.237.55.70:9393/dm04/",
    "http://104.237.55.70:9393/dm05/",
    "http://104.237.55.70:9393/dm06/",
    "http://104.237.55.70:9393/dm07/",
    "http://104.237.55.70:9393/dm08/",
    "http://64.185.235.244:9393/dm09/",
    "http://104.237.55.70:9393/dm10/",
    "http://64.185.235.244:9393/dm11/",
    "http://64.185.235.244:9393/dm12/",
    "http://104.237.55.70:9393/dm13/",
    "http://104.237.55.70:9393/dm14/",
    "http://104.237.55.70:9393/dm15/",
    "http://104.237.55.70:9393/dm16/" ]
    return ServerList

def download_chapter(ch_url, ch_dir):
    content = _download_url(ch_url).decode('gb18030')
    PicListUrl = re.search(r'var PicListUrl = "([^"]+)";', content).group(1)
    PicListUrl=_unsuan(PicListUrl,"tahfcioewrm");
    arrPicListUrl = PicListUrl.split('|');

    server = _getServerList()[int(parse_qs(urlparse(ch_url).query, keep_blank_values=True)['s'][0])-1]

    pairs = []
    for img_path in  arrPicListUrl:
        img_url = server + img_path
        path = os.path.join(ch_dir, img_url[img_url.rindex('/')+1:])
        pairs.append((path, img_url))

    pool = ThreadPool(8)
    pool.map(lambda p:file(p[0], 'wb').write(_download_url(p[1])), pairs)


if not os.path.isdir('book'):
    os.mkdir('book')

for ch in get_chapters(raw_input()):
    subdir = 'book/' + ch['name']
    if not os.path.isdir(subdir):
        os.mkdir(subdir)
    download_chapter(ch['url'], subdir)
