# vim:fileencoding=utf-8

trieTree = dict()
def insert(s):
    for i in range(len(s)):
        _insert(trieTree, s, i)
def _insert(node, s, start):
    for i in range(start, len(s) + 1):
        if i > start:
            node.setdefault('_res', set()).add(s)
        if i < len(s):
            node = node.setdefault(s[i:i+1], dict())
def get(s):
    n = trieTree
    for i in range(len(s)):
        n = n.get(s[i:i+1], None)
        if not n: return None
    return n.get('_res', None)
def clear():
    trieTree = dict()

import re, time

start = time.clock()
for w in re.findall(r'\w+', file('1.txt').read(1<<20)):
    insert(w)
print(time.clock() - start)

while True:
    line = raw_input()
    if not line: break
    print(get(line))
