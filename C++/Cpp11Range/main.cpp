#include "pch.h"

#include <chrono>

template<typename T>
class RangeObj {
public:
    class ConstIterator {
    public:
        ConstIterator(const T &cur, const T &step, int stepCount): mCur(cur), mStep(step), mStepCount(stepCount) {
        }
        T operator * () const {
            return mCur;
        }
        ConstIterator& operator ++() {
            assert(mStepCount > 0);
            mCur += mStep;
            --mStepCount;
            return *this;
        }
        ConstIterator operator ++(int) {
            ConstIterator r(*this);
            ++*this;
            return r;
        }
        bool operator == (const ConstIterator& o) const {
            if (mStepCount == o.mStepCount) {
                return mStepCount == 0 || mCur == o.mCur;
            }
            return false;
        }
        bool operator != (const ConstIterator& o) const {
            return !(*this == o);
        }
    private:
        T mCur;
        T mStep;
        int mStepCount;
    };

public:
    RangeObj(const T &init, const T &step, int size): mInit(init), mStep(step), mSize(size) {
    }
    ConstIterator begin() const { return ConstIterator(mInit, mStep, mSize); }
    ConstIterator end() const { return ConstIterator(mInit + mStep * mSize, mStep, 0); }
    
private:
    T mInit;
    T mStep;
    int mSize;
};

template<typename T>
RangeObj<T> range(T begin, T end, T step = T(1)) {
    int size = max(int(ceil(double(end - begin) / step)), 0);
    return RangeObj<T>(begin, step, size);
}

template<typename T>
RangeObj<T> range(T end) {
    return range(T(), end);
}

class Timer {
public:
    Timer(const char *name): mName(name), mStart(chrono::steady_clock::now()) {
    }
    ~Timer() {
        printf("%s: %.3f\n", mName, chrono::duration<double>(chrono::steady_clock::now() - mStart).count());
    }
private:
    const char *mName;
    chrono::steady_clock::time_point mStart;
};

int main() {
    for (auto i : range(5)) printf("%d,", i);
    puts("");

    for (auto i : range(1, 11)) printf("%d,", i);
    puts("");

    for (auto i : range(1, 1)) printf("%d,", i);
    puts("");

    for (auto i : range(1, -1)) printf("%d,", i);
    puts("");

    for (auto i : range(9, -1, -1)) printf("%d,", i);
    puts("");

    for (auto i : range<float>(3, 0, -0.5)) printf("%.2f,", i);
    puts("");

    const int LOOP = 1024;
    const int N = 1024 * 1024;
    int r = 0;
    {
        Timer _t("for");
        for (int i = 0; i < LOOP; ++i) {
            for (int i = 0; i < N; ++i) r += i & 0x123;
        }
    }

    int r2 = 0;
    {
        Timer _t("range");
        for (int i = 0; i < LOOP; ++i) {
            for (int i : range(N)) r2 += i & 0x123;
        }
    }
    assert(r == r2);

    return r - r2;
}
