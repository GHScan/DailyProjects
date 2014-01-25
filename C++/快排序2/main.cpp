#include <ctime>
#include <cassert>

#include <iostream>
#include <algorithm>
#include <vector>

using namespace std;

template<typename IterT>
IterT partition(IterT begin, IterT end)
{
    IterT k = begin + 1;
    for (IterT _begin = k, _end = end;
            _begin != _end;
            ++_begin) {
        if (*_begin <= *begin) {
            std::swap(*_begin, *k++);
        }
    }
    std::swap(*begin, *--k);
    return k;
}

template<typename IterT>
void quickSort(IterT begin, IterT end)
{
    if (end - begin <= 1) return;
    IterT k = partition(begin, end);
    quickSort(begin, k);
    quickSort(k + 1, end);
}

template<typename IterT>
IterT partition2(IterT begin, IterT end)
{
    assert(begin < end);
    IterT _begin = begin + 1, _end = end - 1;
    while (_begin <= _end) {
        while (_begin <= _end && *_begin <= *begin) ++_begin;
        while (_begin <= _end && *_end > *begin) --_end;
        if (_begin < _end) {
            std::swap(*_begin++, *_end--);
        }
    }
    std::swap(*begin, *_end);
    return _end;
}

template<typename IterT>
void quickSort2(IterT begin, IterT end)
{
    if (end - begin <= 1) return;
    IterT k = partition2(begin, end);
    quickSort2(begin, k);
    quickSort2(k + 1, end);
}

template<typename IterT>
void insertionSort(IterT begin, IterT end)
{
    for (IterT it = begin + 1; 
            it < end; 
            ++it) {
        typename IterT::value_type v = *it;
        IterT it2 = it - 1;
        while (it2 >= begin && *it2 > v) --it2;
        ++it2;
        for (IterT it3 = it; it3 > it2; --it3) {
            it3[0] = it3[-1];
        }
        *it2 = v;
    }
}

template<typename IterT>
IterT partition3(IterT begin, IterT end)
{
    assert(begin < end);
    //if (end - begin < 3) {
    //    return partition2(begin, end);
    //}
    {
        IterT mid = begin + (int)(end - begin) / 2;
        std::swap(*mid, begin[1]);
        mid = begin + 1;

        if (*begin > *mid) std::swap(*begin, *mid);
        if (*begin > end[-1]) std::swap(*begin, end[-1]);
        if (*mid > end[-1]) std::swap(*mid, end[-1]);
        if (*begin == *mid || *mid == end[-1]) return partition2(begin, end);
        std::swap(*begin, *mid);
    }
    IterT _begin = begin + 1, _end = end - 1;
    while (_begin <= _end) {
        while (*_begin <= *begin) ++_begin;
        while (*_end > *begin) --_end;
        if (_begin < _end) {
            std::swap(*_begin++, *_end--);
        }
    }
    std::swap(*begin, *_end);
    return _end;
}

template<typename IterT>
void quickSort3(IterT begin, IterT end)
{
    if (end - begin <= 16) {
        insertionSort(begin, end);
        return;
    }
    // if (end - begin <= 1) return;
    IterT k = partition3(begin, end);
    quickSort3(begin, k);
    quickSort3(k + 1, end);
}

class Timer
{
public:
    Timer(float *p):m_p(p), m_begin(clock()) {}
    ~Timer()
    {
        *m_p += float(clock() - m_begin) / CLOCKS_PER_SEC;
    }
private:
    float *m_p;
    clock_t m_begin;
};

int main()
{
    srand(time(NULL));

    const int LEN = 1 << 19;

    std::vector<int> v;
    for (int i = 0; i < LEN; ++i) v.push_back(rand());
    // for (int i = 0; i < LEN; ++i) v.push_back(i);

    float t0 = 0, t1 = 0, t2 = 0, t3 = 0;

    for (int i = 0; i < 16; ++i) {
        std::vector<int> v0(v);
        {
            Timer _t(&t0);
            std::sort(v0.begin(), v0.end());
        }
        std::vector<int> v1(v);
        {
            Timer _t(&t1);
            quickSort(v1.begin(), v1.end());
        }
        std::vector<int> v2(v);
        {
            Timer _t(&t2);
            quickSort2(v2.begin(), v2.end());
        }
        std::vector<int> v3(v);
        {
            Timer _t(&t3);
            quickSort3(v3.begin(), v3.end());
        }
        assert(v0 == v1);
        assert(v0 == v2);
        assert(v0 == v3);
    }

    cout << "t0 : " << t0 << endl;
    cout << "t1 : " << t1 << endl;
    cout << "t2 : " << t2 << endl;
    cout << "t3 : " << t3 << endl;
}
