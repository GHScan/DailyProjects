# vim:fileencoding=gbk

CHOOSE_LEN = 10
LIST_LEN = 3

def analyze(fname):
    s, l = {}, []
    for line in file(fname).readlines():
        for w in line.split():
            if len(l) == LIST_LEN:
                k = ' '.join(l)
                t = s.setdefault(k, {})
                t[w] = t.get(w, 0) + 1
            l.append(w)
            if len(l) > LIST_LEN:
                l.pop(0)

    s2 = {}
    for k, v in s.iteritems():
        s2[k] = [v[0] for v in sorted(v.items(), None, lambda a: a[1])[0:CHOOSE_LEN]]

    return s2

def loadData(dbFname, dataFname):
    import cPickle
    import os
    if not os.path.isfile(dbFname):
        cPickle.dump(
                analyze(dataFname), file(dbFname, 'wb'), -1)
    return cPickle.load(file(dbFname, 'rb'))

import random

print 'input word length'
n = input()

s2 = loadData('db.txt', '1.txt')
l = random.choice(s2.keys()).split()

print ' '.join(l),
for i in range(n):
    w = random.choice(s2[' '.join(l)])
    print w,
    l.append(w)
    l.pop(0)
