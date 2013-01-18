// vim:fileencoding=gbk

#include "pch.h"

#include <cassert>
#include <ctime>

#include <algorithm>
#include <vector>

class Timer
{
public:
    Timer(clock_t &total):
        m_begin(clock()), m_total(total){ }
    ~Timer()
    {
        m_total += clock() - m_begin;
    }
private:
    clock_t m_begin;
    clock_t &m_total;
};

template<typename T>
void mergeSort(T *begin, T* end)
{
    if (end - begin <= 2) {
        if (end - begin == 2) {
            if (begin[0] > begin[1]) std::swap(begin[0], begin[1]);
        }
        return;
    }

    T *mid = begin + (end - begin) / 2;
    mergeSort(begin, mid);
    mergeSort(mid, end);

    static std::vector<T> temp;
    temp.resize(end - begin);
    T *begin1 = begin, *end1 = mid;
    T *begin2 = mid, *end2 = end;
    T* out = &temp[0];
    while (begin1 < end1 && begin2 < end2) {
        if (*begin1 <= *begin2) *out++ = *begin1++;
        else *out++ = *begin2++;
    }
    while (begin1 < end1) *out++ = *begin1++;
    while (begin2 < end2) *out++ = *begin2++;

    out = &temp[0];
    while (begin < end) *begin++ = *out++;
}

template<typename T>
void backforwardMerge(T *rbegin1, T* rend1, T* rbegin2, T* rend2, T* out)
{
    while (rbegin2 > rend2 && rbegin1 > rend1) {
        if (*rbegin2 >= *rbegin1) *out-- = *rbegin2--;
        else *out-- = *rbegin1--;
    }
    while (rbegin2 > rend2) *out-- = *rbegin2--;
    while (rbegin1 > rend1) *out-- = *rbegin1--;
}

template<typename T>
void _mergeSort2(T *begin, T *end, T *out)
{
    if (end - begin == 1) out[0] = begin[0];
    else if (end - begin == 2) {
        if (begin[0] < begin[1]) {
            out[0] = begin[0];
            out[1] = begin[1];
        }
        else {
            out[0] = begin[1];
            out[1] = begin[0];
        }
    }
    else {
        T *mid = begin + (end - begin + 1) / 2;
        _mergeSort2(begin, mid, out);
        _mergeSort2(mid, end, begin);
        backforwardMerge(
                (begin + (end - mid)) - 1, begin - 1, 
                out + (mid - begin) - 1, out - 1, 
                out + (end - begin) - 1);
    }
}

template<typename T>
void mergeSort2(T *begin, T *end)
{
    static std::vector<T> temp;
    temp.resize((end - begin + 1) / 2);

    T *mid = begin + (end - begin + 1) / 2;
    _mergeSort2(begin, mid, &temp[0]);
    _mergeSort2(mid, end, begin);
    backforwardMerge(
            begin + (end - mid) - 1, begin - 1, 
            &temp[0] + (mid - begin) - 1, &temp[0] - 1, 
            end - 1);
}

int main()
{
    srand((int)time(NULL));

    const int LEN = 1 << 15;
    const int LOOP = 1 << 10;

    std::vector<int> v;
    for (int i = 0; i < LEN; ++i) v.push_back(i);

    {
        /*
        for (int i = 0; i < 100; ++i)
        {
            std::random_shuffle(v.begin(), v.end());
            mergeSort(&v[0], &v[0] + v.size());
            std::random_shuffle(v.begin(), v.end());
            mergeSort2(&v[0], &v[0] + v.size());
        }
        */
    }

    {
        clock_t total = 0;
        for (int i = 0; i < LOOP; ++i)
        {
            std::random_shuffle(v.begin(), v.end());
            {
                Timer t(total);
                mergeSort(&v[0], &v[0] + v.size());
            }
        }
        printf("mergeSort : %f\n", total / float(CLOCKS_PER_SEC));
    }

    {
        clock_t total = 0;
        for (int i = 0; i < LOOP; ++i)
        {
            std::random_shuffle(v.begin(), v.end());
            {
                Timer t(total);
                mergeSort2(&v[0], &v[0] + v.size());
            }
        }
        printf("mergeSort2 : %f\n", total / float(CLOCKS_PER_SEC));
    }

    {
        clock_t total = 0;
        for (int i = 0; i < LOOP; ++i)
        {
            std::random_shuffle(v.begin(), v.end());
            {
                Timer t(total);
                std::stable_sort(&v[0], &v[0] + v.size());
            }
        }
        printf("std::stable_sort : %f\n", total / float(CLOCKS_PER_SEC));
    }

    {
        clock_t total = 0;
        for (int i = 0; i < LOOP; ++i)
        {
            std::random_shuffle(v.begin(), v.end());
            {
                Timer t(total);
                std::sort(&v[0], &v[0] + v.size());
            }
        }
        printf("std::sort : %f\n", total / float(CLOCKS_PER_SEC));
    }
}
