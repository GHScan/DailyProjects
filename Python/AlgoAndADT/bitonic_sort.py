import math


class v0:
    # O(1) in parallel
    @staticmethod
    def kernel_swap(x, ascent):
        half = len(x) // 2
        for i in range(half):
            if (x[i] > x[half + i]) == ascent:
                x[i], x[half + i] = x[half + i], x[i]

    # O(n)
    @staticmethod
    def bitonic_sort(x, ascent):
        if len(x) <= 1: return x
        v0.kernel_swap(x, ascent)
        first = v0.bitonic_sort(x[:len(x) // 2], ascent)
        second = v0.bitonic_sort(x[len(x) // 2:], ascent)
        return first + second

    # O(nlog(n))
    @staticmethod
    def sort(x, ascent):
        if len(x) <= 1: return x
        first = v0.sort(x[:len(x) // 2], True)
        second = v0.sort(x[len(x) // 2:], False)
        return v0.bitonic_sort(first + second, ascent)


class v1:
    # O(1) in parallel
    @staticmethod
    def kernel_swap(x, span, ascent):
        for i in range(len(x)):
            if (i // span) % 2 == 1: 
                continue
            if (x[i] > x[i + span]) == ascent:
                x[i], x[i + span] = x[i + span], x[i]

    # O(log(n))
    @staticmethod
    def bitonic_sort(x, ascent):
        span = len(x) // 2
        while span > 0:
            v1.kernel_swap(x, span, ascent)
            span //= 2
        return x

    # O(n)
    @staticmethod
    def sort(x, ascent):
        if len(x) <= 1: return x
        first = v1.sort(x[:len(x) // 2], True)
        second = v1.sort(x[len(x) // 2:], False)
        return v1.bitonic_sort(first + second, ascent)


class v2:
    # O(1) in parallel
    @staticmethod
    def kernel_swap(x, span, block, ascent):
        for i in range(len(x)):
            if (i // span) % 2 == 1: 
                continue
            direction = ascent if (i // block) % 2 == 0 else not ascent
            if (x[i] > x[i + span]) == direction:
                x[i], x[i + span] = x[i + span], x[i]

    # O(log(n)^2)
    @staticmethod
    def sort(x, ascent):
        block = 2
        while block <= len(x):
            span = len(x) // 2
            while span > 0:
                v2.kernel_swap(x, span, block, ascent)
                span //= 2
            block *= 2
        return x


def test():
    import random

    for sort in [ v0.sort, v1.sort, v2.sort ]:
        for i in range(10):
            a = range(1 << i)
            b = a[:]
            random.shuffle(b)

            a2 = sort(b, True)
            #a2 = list(reversed(sort(b, False)))

            assert a == a2

if __name__ == '__main__':
    test()
