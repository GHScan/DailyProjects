# vim:fileencoding=utf-8

class Trie(object):
    def __init__(self):
        self.m_root = dict()
    def insert(self, s):
        for i in range(len(s)):
            self._insert(self.m_root, s, i)
    def _insert(self, node, s, start):
        for i in range(start, len(s) + 1):
            if i > start:
                node.setdefault(0, set()).add(s)
            if i < len(s):
                node = node.setdefault(s[i:i+1], dict())
    def get(self, s):
        n = self.m_root
        for i in range(len(s)):
            n = n.get(s[i:i+1], None)
            if not n: return None
        return n.get(0, None)
    def clear(self):
        self.m_root = dict()

import re, time

t = Trie()

start = time.clock()
for w in re.findall(r'\w+', file('1.txt').read(1<<20)):
    t.insert(w)
print(time.clock() - start)

while True:
    line = raw_input()
    if not line: break
    print(t.get(line))
