// vim:fileencoding=gbk

#include "pch.h"

#include <cassert>
#include <ctime>

#include <algorithm>

class Timer
{
public:
    Timer(const char *name):
        m_begin(clock()), m_name(name) { }
    ~Timer()
    {
        printf("%s : %f\n", m_name, (clock() - m_begin) / float(CLOCKS_PER_SEC));
    }
private:
    clock_t m_begin;
    const char *m_name;
};

int g_o;
void printArray(int *p, int len)
{
    g_o = len;
    return;
    for (int i = 0; i < len; ++i) printf("%d, ", p[i]);
    puts("");
}

void perm(int *p, int len, int pos = 0)
{
    if (pos == len) {
        printArray(p, len);
        return;
    }
    for (int i = pos; i < len; ++i) {
        std::swap(p[pos], p[i]);
        perm(p, len, pos + 1);
        std::swap(p[pos], p[i]);
    }
}

void perm2(int *p, int len)
{
    struct StackFrame
    {
        int pos;
        int cur;
        StackFrame(int pos) { this->pos = pos; this->cur = pos; }
        StackFrame(){}
    };
    StackFrame stack[32] = {0};
    int top = 0;
    stack[top++] = StackFrame(0);
    while (top > 0) {
        StackFrame& f = stack[top - 1];
        if (f.cur >= len) {
            --top;
            if (top > 0) {
                StackFrame& lf = stack[top - 1];
                std::swap(p[lf.cur], p[lf.pos]);
                lf.cur += 1;
            }
            continue;
        }
        assert(f.cur < len);
        if (f.pos == len - 2) {
            std::swap(p[f.pos], p[f.pos + 1]);
            printArray(p, len);
            std::swap(p[f.pos], p[f.pos + 1]);
            printArray(p, len);
            f.cur = len;
        }
        else {
            std::swap(p[f.pos], p[f.cur]);
            stack[top++] = StackFrame(f.pos + 1);
        }
    }
}

int main()
{
    int a[20] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,};
    const int LEN = 11;

    {
        Timer t("perm");
        perm(a, LEN);
    }
    {
        Timer t("perm2");
        perm2(a, LEN);
    }
}
