#include "pch.h"

#include <list>
#include <set>
#include <unordered_set>

#include <signal.h>
#include <unistd.h>
#include <setjmp.h>

#include "Utils.h"

template<typename T>
static void randomShuffle(T *begin, T *end) {
    int count = end - begin;
    for (int i = count - 1; i > 0; --i) {
        swap(begin[i], begin[myrand(i + 1)]);
    }
}

template<typename T>
static void randomHead(T *begin, T *mid, T *end) {
    int count = end - begin;
    for (int i = 0; i < mid - begin; ++i) {
        swap(begin[i], begin[myrand(i, count)]);
    }
}

template<typename ItT>
static ItT randomChoise(ItT begin, ItT end) {
    ItT r;
    int i = 1;
    for (; begin != end; ++begin) {
        if (myrand(i++) == 0) r = begin;
    }
    return r;
}

static void generateUniqM_set(int *begin, int m, int n) {
    set<int> s;
    while ((int)s.size() < m) s.insert(myrand(n));
    for (int i : s) *begin++ = i;
}

static void generateUniqM_hash(int *begin, int m, int n) {
    unordered_set<int> s;
    while ((int)s.size() < m) s.insert(myrand(n));
    for (int i : s) *begin++ = i;
}

static void generateUniqM_invhash(int *begin, int m, int n) {
    unordered_set<int> s;
    while ((int)s.size() < n - m) s.insert(myrand(n));
    for (int i = 0; i < n; ++i) {
        if (s.count(i) == 0) *begin++ = i;
    }
}

static void generateUniqM_knuth(int *begin, int m, int n) {
    for (int i = 0; i < n && m > 0; ++i) {
        if (myrand(n - i) < m) {
            *begin++ = i;
            --m;
        }
    }
}

static void generateUniqM_floyd(int *begin, int m, int n) {
    unordered_set<int> s;
    for (int i = n - m; i < n; ++i) {
        int t = myrand(i + 1);
        if (s.count(t) > 0) s.insert(i);
        else s.insert(t);
    }
    for (int i : s) *begin++ = i;
}

static void correctnessTest_randomFuncs() {
    // randomShuffle: test uniq
    {
        vector<int> a = {1, 2, 3, 4, 5};
        for (int i = 0; i < 32; ++i) {
            randomShuffle(&a[0], &a[0] + a.size());
            sort(a.begin(), a.end());
            for (int i = 0; i < (int)a.size() - 1; ++i) assert(a[i] < a[i + 1]);
        }
    }
    // randomShuffle: test freq
    {
        for (int i = 0; i < 32; ++i) {
            vector<int> a = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
            vector<int> sum100(a.size(), 0);
            for (int i = 0; i < 100; ++i) {
                for (int i = 0; i < (int)a.size(); ++i) {
                    randomShuffle(&a[0], &a[0] + a.size());
                    for (int i = 0; i < (int)a.size(); ++i) {
                        sum100[i] += a[i];
                    }
                }
            }
            int sum1 = accumulate(a.begin(), a.end(), 0, [](int a, int b){ return a + b;});
            for (int i = 0; i < (int)a.size(); ++i) {
                assert(abs(1 - sum100[i] / (100.0 * sum1)) < 0.1); // 90%
            }
        }
    }

    // randomHead: test uniq
    {
        const int mid = 3;
        vector<int> a = {1, 2, 3, 4, 5};
        for (int i = 0; i < 32; ++i) {
            randomHead(&a[0], &a[0] + mid, &a[0] + a.size());
            sort(a.begin(), a.begin() + mid);
            for (int i = 0; i < mid - 1; ++i) assert(a[i] < a[i + 1]);
        }
    }
    // randomHead: test freq
    {
        for (int i = 0; i < 32; ++i) {
            const int mid = 7;
            vector<int> a = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
            vector<int> sum100(a.size(), 0);
            for (int i = 0; i < 100; ++i) {
                for (int i = 0; i < mid; ++i) {
                    randomHead(&a[0], &a[0] + mid,  &a[0] + a.size());
                    for (int i = 0; i < mid; ++i) {
                        sum100[i] += a[i];
                    }
                }
            }
            double sum1 = accumulate(a.begin(), a.end(), 0.0, [](int a, int b){ return a + b;}) * mid / a.size();
            for (int i = 0; i < mid; ++i) {
                assert(abs(1 - sum100[i] / (100.0 * sum1)) < 0.1); // 90%
            }
        }
    }

    // randomChoise: test uniq
    {
        list<int> a = {1, 2, 3, 4, 5};
        for (int i = 0; i < 32; ++i) {
            for (auto it = a.begin(); it != a.end(); ++it) {
                swap(*it, *randomChoise(it, a.end()));
            }
            a.sort();

            auto it = a.begin();
            auto next = it;
            for (; ++next != a.end(); it = next) {
                assert(*it < *next);
            }
        }
    }
    // randomChoise: test freq
    {
        for (int i = 0; i < 32; ++i) {
            list<int> a = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
            vector<int> sum100(a.size(), 0);
            for (int i = 0; i < 100; ++i) {
                for (int i = 0; i < (int)a.size(); ++i) {
                    for (auto it = a.begin(); it != a.end(); ++it) {
                        swap(*it, *randomChoise(it, a.end()));
                    }

                    auto it = a.begin();
                    for (int i = 0; i < (int)a.size(); ++i, ++it) {
                        sum100[i] += *it;
                    }
                }
            }
            int sum1 = accumulate(a.begin(), a.end(), 0, [](int a, int b){ return a + b;});
            for (int i = 0; i < (int)a.size(); ++i) {
                assert(abs(1 - sum100[i] / (100.0 * sum1)) < 0.1); // 90%
            }
        }
    }
}

struct GenerateUniqFuncItem {
    const char *name;
    void (*f)(int *, int, int);
};

static void correctnessTest_generateUniq(vector<GenerateUniqFuncItem> &funcs) {
    for (auto &func : funcs) {
        int n = 16;
        vector<int> a(n, 0);
        for (int m = 1; m <= n; ++m) {
            func.f(&a[0], m, n);
            sort(a.begin(), a.begin() + m);
            for (int i = 0; i < m - 1; ++i) {
                assert(a[i] >= 0 && a[i] < n);
                assert(a[i] < a[i + 1]);
            }
        }

        int sum = 0;
        for (int i = 0; i < n; ++i) sum += i;
        for (int i = 0; i < 32; ++i) {
            func.f(&a[0], n, n);
            assert(sum == accumulate(a.begin(), a.end(), 0, [](int a, int b){ return a + b; }));
        }
    }
}


sigjmp_buf g_alarmCtx;
static void onSIGALRM_longjmp(int sig) {
    siglongjmp(g_alarmCtx, 1);
}
static bool callWithTimeout(function<void()> f, int second) {
    signal(SIGALRM, &onSIGALRM_longjmp);
    alarm(second);
    if (sigsetjmp(g_alarmCtx, 1) == 0) {
        f();
        alarm(0);
        return true;
    } else {
        // Warning: the local object inside f will not call destructor, so
        // resources leaks will be found without RAII
        return false;
    }
}

static void benchmark_generateUniq(vector<GenerateUniqFuncItem> &funcs) {
    int samples[][2] = {
        {(1<<5), (1<<10)},
        {(1<<10), (1<<10)},
        {(1<<10), (1<<20)},
        {(1<<15), (1<<20)},
        {(1<<20), (1<<20)},
        {(1<<10), (1<<30)},
        {(1<<20), (1<<30)},
    };
    vector<int> a;

    for (auto &sample : samples) {
        int m = sample[0], n = sample[1];
        printf("m=%.3fK,n=%.3fK\n", m / 1024.0, n / 1024.0);
        a.resize(m);

        for (auto &func : funcs) {
            double time = getTime();
            if (callWithTimeout([&](){
                func.f(&a[0], m, n);
            }, 5)) {
                time = getTime() - time;
                printf("\t%s: %fs\n", func.name, time);
            } else {
                printf("\t%s: over 5s !!!!\n", func.name);
            } 
        }
    }
}

int main() { 
    srand(time(nullptr));
    setCpuAffinity(1);

    correctnessTest_randomFuncs();

#define ITEM(f) {#f, f}
    vector<GenerateUniqFuncItem> funcs = {
        ITEM(generateUniqM_set),
        ITEM(generateUniqM_hash),
        ITEM(generateUniqM_invhash),
        ITEM(generateUniqM_knuth),
        ITEM(generateUniqM_floyd),
    };
    correctnessTest_generateUniq(funcs);
    benchmark_generateUniq(funcs);
#undef ITEM
}
