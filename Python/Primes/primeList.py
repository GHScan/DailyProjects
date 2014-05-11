
def primeList(maxN):
    l = [True for i in range(maxN + 1)]
    l[0] = l[1] = False
    for i in range(2, maxN):
        if not l[i]: pass
        else:
            for j in range(2*i,maxN,i):
                l[j] = False
    return [i for (i,b) in zip(range(2,maxN), l[2:]) if b]

print primeList(100)
