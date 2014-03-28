import random, time, sys

def _insertionSort(a, begin, end):
    for i in range(begin + 1, end):
        v = a[i]
        j = i - 1
        while j >= 0 and a[j] > v: 
            a[j + 1] = a[j]
            j -= 1
        a[j + 1] = v
    return a
def insertionSort(a):
    _insertionSort(a, 0, len(a))
    return a

def quickSort_builtin(a):
    a.sort()
    return a

def quickSort_copy_2way(a):
    if len(a) <= 1: return a
    return quickSort_copy_2way([v for v in a[1:] if v <= a[0]]) + a[0:1] + quickSort_copy_2way([v for v in a[1:] if v > a[0]])

def quickSort_copy_3way(a):
    if len(a) <= 1: return a
    l = quickSort_copy_3way([v for v in a[1:] if v < a[0]])
    m = [v for v in a[1:] if v == a[0]] + a[0:1]
    r = quickSort_copy_3way([v for v in a[1:] if v > a[0]])
    return l + m + r

def quickSort_copy_2way_swap(a):
    if len(a) <= 1: return a
    mid = len(a) / 2
    a[0], a[mid] = a[mid], a[0]
    return quickSort_copy_2way_swap([v for v in a[1:] if v <= a[0]]) + a[0:1] + quickSort_copy_2way_swap([v for v in a[1:] if v > a[0]])

def quickSort_copy_3way_swap(a):
    if len(a) <= 1: return a
    mid = len(a) / 2
    a[0], a[mid] = a[mid], a[0]
    l = quickSort_copy_3way_swap([v for v in a[1:] if v < a[0]])
    m = [v for v in a[1:] if v == a[0]] + a[0:1]
    r = quickSort_copy_3way_swap([v for v in a[1:] if v > a[0]])
    return l + m + r

def _quickSort(a, begin, end):
    if end - begin <= 1: return 
    m = begin
    for i in range(begin + 1, end):
        if a[i] <= a[begin]:
            ++m
            a[i], a[m] = a[m], a[i] 
    a[begin], a[m] = a[m], a[begin]
    _quickSort(a, begin, m)
    _quickSort(a, m + 1, end)
def quickSort(a):
    _quickSort(a, 0, len(a))
    return a

def _quickSort_insertion(a, begin, end):
    if end - begin <= 8: 
        _insertionSort(a, begin, end)
        return 

    m = begin
    for i in range(begin + 1, end):
        if a[i] <= a[begin]:
            ++m
            a[i], a[m] = a[m], a[i] 
    a[begin], a[m] = a[m], a[begin]
    _quickSort_insertion(a, begin, m)
    _quickSort_insertion(a, m + 1, end)
def quickSort_insertion(a):
    _quickSort_insertion(a, 0, len(a))
    return a

def _mergeRCombine(a, b):
    if not a or not b: return a or b
    return a[0:1] + _mergeRCombine(a[1:], b) if a[0] < b[0] else b[0:1] + _mergeRCombine(a, b[1:])
def mergeSort_copy_rCombine(a):
    if len(a) <= 1: return a
    mid = len(a) / 2
    l = mergeSort_copy_rCombine(a[:mid])
    r = mergeSort_copy_rCombine(a[mid:])
    return _mergeRCombine(l, r)

def mergeSort_copy_combine(a):
    if len(a) <= 1: return a
    mid = len(a) / 2
    l = mergeSort_copy_combine(a[:mid])
    r = mergeSort_copy_combine(a[mid:])
    out = []
    while l and r: 
        if l[0] <= r[0]:
            out.append(l[0])
            l = l[1:]
        else:
            out.append(r[0])
            r = r[1:]
    return out + l + r

##############################

def correctnessTest(funcs):
    for l in range(0, 128):
        a = []
        while len(a) < l: a.append(random.randint(0, l))

        for f, limit in funcs:
            aa = list(a)
            assert f(aa) == sorted(a), '%s: %s' % (f.func_name, str(a))

def random_shuffle(a):
    random.shuffle(a)
    return a

def benchmark(funcs):
    datas = []
    datas.append(('ordered', list(range(1024))))
    datas.append(('repeat', random_shuffle([2] * 1024)))
    datas.append(('32', random_shuffle(range(2**5))))
    datas.append(('1K', random_shuffle(range(2**10))))
    datas.append(('4K', random_shuffle(range(2**12))))
    datas.append(('32K', random_shuffle(range(2**15))))
    datas.append(('512K', random_shuffle(range(2**19))))
    for data in datas:
        print data[0], ':'
        for func, limit in funcs:
            if limit and len(data[1]) > limit: continue

            minT = 2**10;
            for i in range(3):
                d = list(data[1])
                t = time.time()
                func(d)
                t = time.time() - t
                minT = min(minT, t)
            
            print '\t%s: %.6f' % (func.func_name, minT)

##############################

sys.setrecursionlimit(2**15)

funcs = [
        (insertionSort, 1*1024),
        (quickSort_builtin, 0),
        (quickSort_copy_2way, 32*1024),
        (quickSort_copy_2way_swap, 32*1024),
        (quickSort_copy_3way, 32*1024),
        (quickSort_copy_3way_swap, 32*1024),
        (quickSort, 1*1024),
        (quickSort_insertion, 1*1024),
        (mergeSort_copy_rCombine, 4*1024),
        (mergeSort_copy_combine, 4*1024),
        ]

correctnessTest(funcs)
print 'correctnessTest finish!'

benchmark(funcs)
