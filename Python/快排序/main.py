def partition(array, begin, end):
    assert end > begin
    t = array[begin]
    j = begin + 1
    for i in range(begin + 1, end):
        if array[i] <= t:
            array[i], array[j] = array[j], array[i]
            j += 1
    j -= 1
    array[begin], array[j] = array[j], array[begin]
    return j

def quick_sort(array, begin = 0, end = None):
    if end == None: end = len(array)
    if end - begin < 2: return
    k = partition(array, begin, end)
    quick_sort(array, begin, k)
    quick_sort(array, k + 1, end)

def partition2(array, begin, end):
    assert end > begin
    t = array[begin]
    start, last = begin + 1, end - 1
    while start <= last:
        while start <= last and array[start] <= t:
            start += 1
        while start <= last and array[last] > t:
            last -= 1
        if start < last:
            array[start], array[last] = array[last], array[start]
            start += 1
            last -= 1
    array[last], array[begin] = array[begin], array[last]
    return last

def quick_sort2(array, begin = 0, end = None):
    if end == None: end = len(array)
    if end - begin < 2: return
    k = partition2(array, begin, end)
    quick_sort2(array, begin, k)
    quick_sort2(array, k + 1, end)


import random
for i in range(100):
    a = list(random.randint(1, 1000) for i in range(1000))
    b = list(a)
    quick_sort2(a)
    b.sort()
    assert a == b
