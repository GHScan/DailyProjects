# vim:fileencoding=gbk

import time

def info(obj, callMethod = False):
    '''打印对象信息'''

    if not obj : return 'empty object'
    funcStrs = []
    fieldStrs = []
    for attrName in dir(obj):
        attr = getattr(obj, attrName)
        attrName = attrName.ljust(16, ' ')
        if callable(attr):
            if callMethod:
                try:
                    ret = attr()
                except: 
                    ret = 'call failed!'
                funcStrs.append('%s : %s' % (attrName, ret))
            else:
                doc = attr.__doc__[:60] if attr.__doc__ else ''
                funcStrs.append('%s : %s' % (attrName, doc))
        else:
            fieldStrs.append('%s : %s' % (attrName, attr))
    return 'object:\n%s\nfields:\n%s\nmethods:\n%s' % (
            obj, '\n'.join(fieldStrs), '\n'.join(funcStrs))

class Timer(object):
    def __init__(self, name = ''):
        self._name = name
    def __enter__(self):
        self._start = time.time()
        return self
    def __exit__(self, *args):
        if __debug__ :
            print 'timer(%s) : %f' % (self._name, time.time() - self._start)
        return None

class ExitScope(object):
    def __init__(self, onExit):
        self._onExit= onExit
    def __enter__(self):
        return self
    def __exit__(self, *args):
        self._onExit()
        return None
        
def unique(l):
    s = set()
    for i in l:
        if i not in s:
            s.add(i)
            yield i
