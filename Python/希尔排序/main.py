def insertion_sort(array, off, dis):
    for i in range(off + dis, len(array), dis):
        j = i - dis
        while j >= 0 and array[j] > array[i]:
            j -= dis
        j += dis
        t = array[i]
        for k in range(i, j, -dis):
            array[k] = array[k - dis]
        array[j] = t


def shell_sort(array):
    dis_array = [1, 4, 13, 40, 121, 363, 1089, 3267, 9861, 29643,]
    l = len(array)
    assert l > 0

    off = len(dis_array) - 1
    while dis_array[off] >= l:
        off -= 1
    for i in range(off, -1, -1):
        dis = dis_array[i]
        for j in range(0, dis):
            insertion_sort(array, j, dis)

import random
for i in range(100):
    a = list(random.randint(1, 1000) for i in range(1000))
    b = list(a)
    shell_sort(a)
    b.sort()
    assert a == b
