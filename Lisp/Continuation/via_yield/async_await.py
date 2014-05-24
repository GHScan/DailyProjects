# similar with coroutine
#------------------------------
import random
#------------------------------
class Task(object):
    def __init__(self, f, *args):
        args = list(args)
        self._callback, args[len(args) - 1] = args[len(args) - 1], self
        self._topGen = f(*args)
        self._resume()
    def await(self, f, *args):
        value = []
        def _fcallback(v):
            value.append(v)
            self._resume()
        args += (_fcallback,)
        f(*args)
        yield (False, False)
        yield (True, value[0])
    def aret(self, v):
        self._callback(v)
        yield (False, False)
    def _resume(self):
        next(self._topGen, None)
#------------------------------
_events = []
_now = 0
def time():
    return _now
def dispatchEvent():
    if len(_events):
        e = _events.pop(0)
        global _now
        _now = e[0]
        e[1]()
        return True
    else:
        return False
def insertEvent(delay, callback):
    finish = time() + delay
    i = 0
    while i < len(_events) and _events[i][0] <= finish:
        i += 1
    _events.insert(i, [finish, callback])
#------------------------------
def connect(host, port, callback):
    insertEvent(random.randint(5, 10), lambda : callback('conn'))
def send(conn, data):
    pass
def receive(conn, callback):
    insertEvent(random.randint(1, 4), lambda : callback('response:' + str(time())))
def close(conn):
    pass
#------------------------------
def requestUrl(url, port, task):
    for _, conn in task.await(connect, url, port): 
        if not _: yield (_, conn)
    print '[%d]%s: connected' % (time(), url)

    send(conn, 'GET / HTTP/1.0')
    for i in range(10):
        for _, tmp in task.await(receive, conn): 
            if not _: yield (_, tmp)
        print '[%d]%s: receive tmp' % (time(), url)
        send(conn, 'request:' + tmp)
    
    for _, html in task.await(receive, conn): 
        if not _: yield (_, html)
    print '[%d]%s: receive html' % (time(), url)

    close(conn)
    print '[%d]%s: close' % (time(), url)

    for _ in task.aret(html): yield _
#------------------------------
def requestUrlsSync(urls, task):
    for url in urls:
        for _, ret in task.await(Task, requestUrl, url, 80): 
            if not _: yield (_, ret)
    for _ in task.aret(0): yield _
def requestUrlsAsync(urls, callback):
    n = [0]
    def _requestCallback(ignore):
        n[0] += 1
        if n[0] == len(urls):
            callback(0)

    for url in urls:
        Task(requestUrl, url, 80, _requestCallback)
#------------------------------
urls = ['www.baidu.com', 'www.taobao.com', 'www.sina.com', 'www.qq.com']
def onFinish(ignore): print 'finish all!'
#Task(requestUrlsSync, urls, onFinish)
requestUrlsAsync(urls, onFinish)
while dispatchEvent(): pass
