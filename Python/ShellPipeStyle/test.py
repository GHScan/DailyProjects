
class Pipe(object):
    def __init__(self, func):
        self.m_func = func
    def __call__(self, *args):
        self.m_args = args
        return self
    def __ror__(self, datas):
        return self.m_func(datas, *self.m_args)

@Pipe
def multipe_by(datas, n=2):
    for d in datas: yield d * n
@Pipe
def echo(datas):
    for d in datas: print d; yield d
@Pipe
def head(datas, n=1):
    for _, d in zip(range(n), datas): yield d
@Pipe
def tail(datas, n=1):
    for _, d in reversed(zip(range(n), reversed(list(datas)))): yield d

res = range(10) | multipe_by(3) | head(5) | tail(3) | echo()
list(res)
