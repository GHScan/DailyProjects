import sys, time
import signal

def readInputs():
    while True:
        s = sys.stdin.readline()
        if not s: break
        yield s

def withEmpty(words):
    start = time.time()
    for w in words:
        pass
    print '%fs' % (time.time() - start)

    signal.pause()

def withDict(words):
    start = time.time()
    d = dict()
    for w in words:
        d[w] = d.get(w, 0) + 1
    print '%fs' % (time.time() - start)

    signal.pause()

def withDefaultdict(words):
    import collections

    start = time.time()
    d = collections.defaultdict(int)
    for w in words:
        d[w] += 1
    print '%fs' % (time.time() - start)

    signal.pause()

globals()['with' + sys.argv[1].capitalize()](readInputs())
