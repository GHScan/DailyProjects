#ifndef HELPER_H
#define HELPER_H

#ifdef __GNUC__
#define RESTRICT __restrict__
#else
#define RESTRICT __restrict
#endif

class Char2DigitTable {
public:
    static Char2DigitTable* instance() {
        static Char2DigitTable s_ins;
        return &s_ins;
    }
    int toDigit(unsigned char c, int base) const {
        assert(base >= 2 && base <= 36);
        int r = mTable[c];
        return r >= 0 && r < base ? r : -1;
    }
    static char toChar(int i) {
        assert(i >= 0 && i < 36);
        return i < 10 ? i + '0' : i - 10 + 'a';
    }
private:
    Char2DigitTable() {
        memset(mTable, -1, sizeof(mTable));
        for (int i = 0; i < 10; ++i) mTable[i + '0'] = i;
        for (int i = 0; i < 26; ++i) mTable[i + 'A'] = mTable[i + 'a'] = i + 10;
    }
    Char2DigitTable(const Char2DigitTable&) = delete;
    Char2DigitTable& operator = (const Char2DigitTable&) = delete;
private:
    char mTable[256];
};

template<typename T, int initN>
class SmallVector {
    static_assert(is_pod<T>::value, "");
public:
    SmallVector(): mBuf(mStaticBuffer), mCapacity(initN), mSize(0) { 
    }
    explicit SmallVector(int size, const T &init = T()): SmallVector() {
        resize(size, init);
    }
    ~SmallVector() { 
        clear();
    }
    SmallVector(const SmallVector& o): SmallVector() {
        *this = o;
    }
    SmallVector& operator = (const SmallVector& o) {
        if (this != &o) {
            resize(o.mSize);
            memcpy(mBuf, o.mBuf, o.mSize * sizeof(T));
        }
        return *this;
    }
    SmallVector(SmallVector&& o): SmallVector() {
        *this = move(o);
    }
    SmallVector& operator = (SmallVector&& o) {
        if (this != &o) {
            if (o.isDynamicBuffer() && isDynamicBuffer() && mCapacity < o.mCapacity) {
                clear();
            }

            if (o.isDynamicBuffer() && !isDynamicBuffer()) {
                mCapacity = o.mCapacity;
                mBuf = o.mBuf;
                mSize = o.mSize;
                o.mCapacity = initN;
                o.mBuf = o.mStaticBuffer;
                o.mSize = 0;
            } else {
                // call operator=(const SmallVector&)
                *this = o;
            }
        }
        return *this;
    }

    T& operator [] (int i) { assert(i >= 0 && i < mSize); return mBuf[i]; }
    const T& operator [] (int i) const { assert(i >= 0 && i < mSize); return mBuf[i]; }
    const T& back() const { assert(mSize > 0); return mBuf[mSize - 1]; }
    T& back() { assert(mSize > 0); return mBuf[mSize - 1]; }
    const T& front() const { assert(mSize > 0); return mBuf[0]; }
    T& front() { assert(mSize > 0); return mBuf[0]; }
    void push_back(const T &v) { reserve(mSize + 1); mBuf[mSize++] = v; }
    void pop_back() { assert(mSize > 0); --mSize; }
    T* ptr() { return mBuf; }
    const T* ptr() const { return mBuf; }
    int size() const { return mSize; }
    bool isDynamicBuffer() const { return mBuf != mStaticBuffer;}
    void resize(int size, const T &init = T()) {
        if (size > mSize) {
            reserve(size);
            for (int i = mSize; i < size; ++i) mBuf[i] = init;
        }
        mSize = size;
    }
    void reserve(int capacity) {
        if (capacity <= mCapacity) return;
        if (isDynamicBuffer()) {
            if (T *p = (T*)realloc(mBuf, capacity * sizeof(T))) mBuf = p;
            else assert(0);
        } else {
            mBuf = (T*)malloc(capacity * sizeof(T));
            if (mBuf == nullptr) assert(0);
            else memcpy(mBuf, mStaticBuffer, sizeof(mStaticBuffer));
        }
        mCapacity = capacity;
    }
    void clear() {
        mSize = 0;
        if (isDynamicBuffer()) {
            ::free(mBuf);
            mBuf = mStaticBuffer;
            mCapacity = initN;
        }
    }
private:
    T *mBuf;
    int mCapacity;
    int mSize;
    T mStaticBuffer[initN];
};

#endif
