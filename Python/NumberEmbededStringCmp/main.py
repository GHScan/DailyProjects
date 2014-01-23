# vim:fileencoding=utf-8

l = [
        'abc',
        'ab9c',
        '',
        'ab10c',
        'ab20c33',
        'ab8c',
        'ab10a',
        '_',
        '3223aab',
        'ab',
        'ab20c8e',
        '',
        'ab10d',
        ]

import re
conv = re.compile(r'\d+|\D*')
def mycmp(la, lb):
    la, lb = conv.findall(la), conv.findall(lb)
    for a, b in zip(la, lb):
        c = cmp(int(a), int(b)) if (a.isdigit() and b.isdigit()) else cmp(a, b)
        if c: return c
    return 0

print sorted(l, mycmp)
