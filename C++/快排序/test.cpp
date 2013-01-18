// vim:fileencoding=gbk

#include <time.h>

#include <algorithm>
#include <vector>

void insertion_sort(int *begin, int *end)
{
    for (int *cur = begin + 1; cur < end; ++cur) {
        int *smaller = cur - 1;
        while (smaller >= begin && *cur < *smaller) --smaller;
        int val = *cur;
        for (int *p = cur; p > smaller + 1; --p) p[0] = p[-1];
        smaller[1] = val;
    }
}

void sort(int *begin, int *end)
{
    int dis = int(end - begin);
    if (dis <= 1) return;
    else if (dis < 16) {
        insertion_sort(begin, end);
        return;
    }
    else {}

    {
        int *mid = begin + (end - begin) / 2;
        std::swap(*mid, *begin);
        if (begin[0] > end[-1]) std::swap(begin[0], end[-1]);
        ++end[-1];
    }

    int ref = *begin;
    int *min = begin + 1;
    int *max = end - 1;
    while (min <= max) {
        while (*min <= ref) ++min;
        while (*max > ref) --max;
        if (min < max) {
            std::swap(*min, *max);
            ++min, --max;
        }
    }

    std::swap(*begin, *max);
    --end[-1];
    sort(begin, max);
    sort(max + 1, end);
}

class Timer
{
public:
    Timer(const char *name): m_name(name)
    {
        m_start = clock();
    }
    ~Timer() 
    {
        cout << m_name << ":" << (clock() - m_start) / 1000.0 << endl;
    }
private:
    const char *m_name;
    clock_t m_start;
};

int qsort_compare(const void *a, const void *b)
{
    return *(const int*)a - *(const int *)b;
}

int main()
{
    srand((unsigned)time(NULL));

    const int LEN = 1 << 22;
    std::vector<int> a(LEN, 0);
    for (int i = 0; i < LEN; ++i) a[i] = i;

    for (int i = 0; i < 4; ++i) {
        std::random_shuffle(a.begin(), a.end());
        std::vector<int> v(a.begin(), a.end());
        std::vector<int> v2(a.begin(), a.end());
        {
            Timer t("my sort");
            sort(&a[0], &a[0] + a.size());
        }
        {
            Timer t("std sort");
            std::sort(v.begin(), v.end());
        }
        {
            Timer t("qsort");
            qsort(&v2[0], v2.size(), sizeof(int), qsort_compare);
        }
        bool  b = std::equal(a.begin(), a.end(), v.begin());
        if (!b) cout << "error!!!!!!!!!" << endl;
    }
}
