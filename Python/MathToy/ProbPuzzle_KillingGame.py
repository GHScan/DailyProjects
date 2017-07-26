#vim:fileencoding=utf-8

# 600个人站一排，每次随机杀掉一个奇数位的人，几号最安全？
# 最安全参考两种定义方式:
# 杀死需要的次数的期望值最大。
# 成为最后一个被杀死的概率最大。

def plot(data):
    import matplotlib.pyplot as plt
    plt.plot(data)
    plt.show()

def calc_survival_prob(particpants, survivals):
    a = [1] * survivals

    for n in range(survivals + 1, particpants + 1):
        a2 = []

        kn = (n + 1) // 2

        for i in range(n-1):
            kl = (i + 1) // 2
            kr = kn - kl - (1 if i%2==0 else 0)
            pl, pr = kl / kn, kr / kn
            p = pl * a[i-1] + pr * a[i]
            a2.append(p)

        a2.append((n // 2 / kn) * a[-1])

        a = a2

    plot(a)

def calc_survival_turn_expect(particpants, survivals):
    a = [1] * survivals

    for n in range(survivals + 1, particpants + 1):
        a2 = []

        kn = (n + 1) // 2

        for i in range(n-1):
            kl = (i + 1) // 2
            km = 1 if i%2==0 else 0
            kr = kn - kl - km
            pl, pm, pr = kl / kn, km / kn, kr / kn
            e = pm + pl * (a[i-1] + 1) + pr * (a[i] + 1)
            a2.append(e)

        a2.append((n // 2 / kn) * (1 + a[-1]) + ((1 if (n-1)%2==0 else 0) / kn))
    
        a = a2

    plot(a)

calc_survival_prob(600, 1)
#calc_survival_turn_expect(600, 1)
