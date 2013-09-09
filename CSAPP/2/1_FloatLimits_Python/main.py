#! /usr/bin/env python

import random

nums = [float(5) / i for i in range(10000000, 9000000, -1)]

print sum(nums)

nums.append(9876500000)
print sum(nums)

print sum(reversed(nums))

random.shuffle(nums)
print sum(nums)
