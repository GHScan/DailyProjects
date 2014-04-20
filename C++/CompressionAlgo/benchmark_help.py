#! /usr/bin/env python

import re, sys

lines = [l.strip() for l in sys.stdin.readlines() if l.strip()]

m = re.search(r'(\d\.\d+)s', lines[0])
compressT = float(m.group(1))

m = re.search(r'(\d\.\d+)s', lines[3])
uncompressT = float(m.group(1))

m = re.search(r'^([\d\.]+)', lines[6])
originSize = float(m.group(1)) / 1000000

m = re.search(r'^([\d\.]+)', lines[7])
compressedSize = float(m.group(1)) / 1000000

print 'rate=%.2f%%, compress=%.1fM/s, uncompress=%.fM/s' % (compressedSize * 100 / originSize, originSize / compressT, originSize / uncompressT)
