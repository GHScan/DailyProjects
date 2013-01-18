// vim:fileencoding=gbk

#include "pch.h"

#include <ctime>
#include <cassert>

#include <vector>
#include <utility>

int g_o;
void printArray(int out[], int len)
{
    g_o = len;
    return;
    printf("[");
    for (int i = 0; i < len; ++i) printf("%d,", out[i]);
    puts("]");
}

void func(int value, int n, int out[], int top = 0, int init = 1)
{
    assert(n >= 1);
    if (n == 1) {
        if (value >= init) {
            out[top] = value;
            printArray(out, top + 1);
        }
        return;
    }
    for (int i = init; i <= value; ++i) {
        out[top] = i;
        func(value - i, n - 1, out, top + 1, i);
    }
}

void func1(int value, int n, int out[], int top = 0, int init = 1)
{
    assert(n >= 1);
    if (n == 1) {
        if (value >= init) {
            out[top] = value;
            printArray(out, top + 1);
        }
        return;
    }
    int max = value / n + 1;
    for (int i = init; i <= max; ++i) {
        out[top] = i;
        func(value - i, n - 1, out, top + 1, i);
    }
}

void func2(int value, int n, int out[], int top = 0, int init = 1)
{
    assert(n >= 2);
    if (n == 2) {
        int max = value >> 1;
        for (int i = init; i <= max; ++i) {
            out[top] = i;
            out[top + 1] = value = i;
            printArray(out, top + 2);
        }
        return;
    }
    for (int i = init; i <= value; ++i) {
        out[top] = i;
        func(value - i, n - 1, out, top + 1, i);
    }
}

void func3(int value, int n)
{
    assert(n <= 10);
    int out[10] = {0};
    int values[10] = {value};
    typedef std::vector<std::pair<int, int> > Vec;
    Vec stack;
    stack.reserve(value);
    for (int i = 1; i <= value; ++i) {
        stack.push_back(Vec::value_type(0, i));
    }
    while (!stack.empty()) {
        Vec::value_type val = stack.back();
        stack.pop_back();
        out[val.first] = val.second;
        values[val.first + 1] = values[val.first] - val.second;
        if (val.first == n - 2) {
            if (values[val.first + 1] >= val.second) {
                out[val.first + 1] = values[val.first + 1];
                printArray(out, n);
            }
            continue;
        }
        int min = val.second;
        int max = values[val.first + 1];
        for (int i = min; i <= max; ++i) {
            stack.push_back(Vec::value_type(val.first + 1, i));
        }
    }
}

void func4(int value, int n)
{
    assert(n <= 10);
    struct StackFrame
    {
        int value, top, apply;
        StackFrame(int value, int top, int apply)
        {
            this->value = value;
            this->top = top;
            this->apply = apply;
        }
    };
    int out[10] = {0};
    std::vector<StackFrame> stack;
    stack.reserve(value);
    stack.push_back(StackFrame(value, 0, 0));
    while (!stack.empty()) {
        const StackFrame& frame = stack.back();
        int val = frame.value, top = frame.top;
        if (frame.top > 0) {
            out[frame.top - 1] = frame.apply;
        }
        stack.pop_back();
        if (top == n - 1) {
            if (val >= out[top - 1]) {
                out[top] = val;
                printArray(out, n);
            }
            continue;
        }
        int min = top > 0 ? out[top - 1] : 1;
        int max = val;
        for (int i = min; i <= max; ++i) {
            stack.push_back(StackFrame(val - i, top + 1, i));
        }
    }
}

void func5(int value, int n)
{
    assert(n <= 10);
    struct StackFrame
    {
        int value;
        int top;
        int cur;
        StackFrame(int value, int top, int cur)
        {
            this->value = value;
            this->top = top;
            this->cur = cur;
        }
    };
    int out[10] = {0};
    std::vector<StackFrame> stack;
    stack.reserve(value);
    stack.push_back(StackFrame(value, 0, 1));
    while (!stack.empty()) {
        StackFrame& frame = stack.back();
        if (frame.top == n - 2) {
            int max = frame.value >> 1;
            while (frame.cur <= max) {
                out[frame.top] = frame.cur;
                out[frame.top + 1] = frame.value - frame.cur;
                printArray(out, n);
                ++frame.cur;
            }
            stack.pop_back();
            continue;
        }
        if (frame.cur > frame.value) {
            stack.pop_back();
            continue;
        }
        stack.push_back(StackFrame(
                    frame.value - frame.cur, frame.top + 1, frame.cur));
        out[frame.top] = frame.cur;
        ++frame.cur;
    }
}

void func6(int value, int n)
{
    struct StackFrame6
    {
        int value;
        int cur;
        StackFrame6(){}
        StackFrame6(int value, int cur)
        {
            this->value = value;
            this->cur = cur;
        }
    };
    assert(n <= 10);
    int out[10] = {0};
    StackFrame6 stack[5];
    int stackTop = 0;
    stack[stackTop++] = StackFrame6(value, 1);
    while (stackTop > 0) {
        int top = stackTop - 1;
        StackFrame6& frame = stack[top];
        if (top == n - 2) {
            int max = frame.value >> 1;
            while (frame.cur <= max) {
                out[top] = frame.cur;
                out[top + 1] = frame.value - frame.cur;
                printArray(out, n);
                ++frame.cur;
            }
            --stackTop;
            continue;
        }
        if (frame.cur > frame.value) {
            --stackTop;
            continue;
        }
        stack[stackTop++] = StackFrame6(
                frame.value - frame.cur, frame.cur);
        out[top] = frame.cur;
        ++frame.cur;
    }
}

void func7(int value, int n)
{
    struct StackFrame7
    {
        int value;
        int cur;
        int max;
        StackFrame7(int value, int cur, int max)
        {
            this->value = value;
            this->cur = cur;
            this->max = max;
        }
        StackFrame7(){}
    };
    assert(n <= 10);
    int out[10] = {0};
    StackFrame7 stack[5];
    int stackTop = 0;
    stack[stackTop++] = StackFrame7(value, 1, value / n + 1);
    while (stackTop > 0) {
        int top = stackTop - 1;
        StackFrame7& frame = stack[top];
        if (top == n - 2) {
            int max = frame.value >> 1;
            while (frame.cur <= max) {
                out[top] = frame.cur;
                out[top + 1] = frame.value - frame.cur;
                printArray(out, n);
                ++frame.cur;
            }
            --stackTop;
            continue;
        }
        if (frame.cur > frame.max) {
            --stackTop;
            continue;
        }
        stack[stackTop++] = StackFrame7(
                frame.value - frame.cur, frame.cur, (frame.value - frame.cur) / (n - top - 1));
        out[top] = frame.cur;
        ++frame.cur;
    }
}
int main()
{
    int a[10] = {0};
    clock_t c = clock();
    // func5(6, 3);
    // func5(8, 3);
    // func2(10000, 3, a);
    func5(30000, 3);
    printf("%f\n", (clock() - c) / float(CLOCKS_PER_SEC));
    printf("%d\n", g_o);
}
