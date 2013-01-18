# vim:fileencoding=gbk

import random

word_str = '''
句子 -sentence
'''

test = [tuple(l.split('-', 2)) for l in word_str.splitlines() if l]
test += [(v, k) for k, v in test]
random.shuffle(test)
test += test[:len(test) / 3]
random.shuffle(test)

for k, v in test:
    raw_input(k + ' -> ')
    print '\t', v
