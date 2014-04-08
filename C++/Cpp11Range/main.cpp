#include "pch.h"

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
}
