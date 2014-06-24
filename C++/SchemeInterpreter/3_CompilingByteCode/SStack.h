#ifndef SSTACK_H
#define SSTACK_H

#include "SValue.h"

struct SScriptFunction;
struct SEnv;
class SInterpreter;

class SEvalStack {
public:
    SEvalStack(int initCapactiy);
    ~SEvalStack();
    SEvalStack(const SEvalStack&) = delete;
    SEvalStack& operator = (const SEvalStack&) = delete;

    SValue* alloc() {
        if (mSize + 1 > mCapacity) growCapacity();
        ASSERT(mSize + 1 <= mCapacity);

        return mBasePtr + mSize++;
    }

    void push(SValue v) {
        if (mSize + 1 > mCapacity) growCapacity();
        ASSERT(mSize + 1 <= mCapacity);

        mBasePtr[mSize++] = v;
    }

    SValue& top(int n) {
        ASSERT(n < 0);
        return mBasePtr[mSize + n];
    }

    SValue pop() {
        ASSERT(mSize >= 1);
        return mBasePtr[--mSize];
    }

    void pop(int n) {
        ASSERT(mSize >= n);
        mSize -= n;
    }

    int size() const {
        return mSize;
    }

    SValue* begin() {
        return mBasePtr;
    }

    SValue* end() {
        return mBasePtr + mSize;
    }

private:
    void growCapacity();

private:
    SValue *mBasePtr;
    int mSize;
    int mCapacity;
};

class SFrameStack {
public:
    struct StackFrame {
        SScriptFunction *func;
        SEnv *localEnv;
        int pc;
    };

public:
    SFrameStack() = default;
    ~SFrameStack() = default;

    SFrameStack(const SFrameStack&) = delete;
    SFrameStack& operator = (const SFrameStack&) = delete;

    StackFrame* allocFrame() {
        mFrames.push_back(StackFrame{nullptr, nullptr, 0});
        return &mFrames.back();
    }

    StackFrame* top() {
        return &mFrames.back();
    }

    void pop() {
        mFrames.pop_back();
    }

    int size() const {
        return (int)mFrames.size();
    }

    vector<StackFrame>::iterator begin() {
        return mFrames.begin();
    }

    vector<StackFrame>::iterator end() {
        return mFrames.end();
    }

private:
    vector<StackFrame> mFrames;
};

#endif
