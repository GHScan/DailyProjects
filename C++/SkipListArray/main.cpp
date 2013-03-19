
#include "pch.h"

#include <time.h>
#include <assert.h>

#include <list>
#include <deque>

#include "SkipListArray.h"

class Timer
{
public:
    Timer(const char *name): m_name(name), m_c(clock()){}
    ~Timer()
    {
        cout << m_name << ":" << (clock() - m_c) / 1000.0f << endl;
    }
private:
    const char *m_name;
    clock_t m_c;
};

int myrand() 
{
    return (rand() << 15) | rand();
}

void performanceTest()
{
    const int DATA_N = 1 << 20;
    const int ACT_N = 1 << 16;
//#define TEST_LIST
//#define TEST_DEQUE

    vector<int> rands;
    for (int i = 0; i < ACT_N; ++i) rands.push_back(myrand());

    vector<int> res, res2;

    {
        vector<int> v;
        puts("********** test vector");
        {
            Timer _t("vector build:");
            for (int i = 0; i < DATA_N; ++i) v.push_back(i);
        }
        {
            Timer _t("vector action:");
            for (auto rn : rands) {
                if (rn % 2) {
                    int off = rn % (v.size() + 1);
                    v.insert(v.begin() + off, rn);
                }
                else {
                    int off = rn % v.size();
                    v.erase(v.begin() + off);
                }
            }
        }
        res.assign(v.begin(), v.end());
    }

#ifdef TEST_LIST
    {
        list<int> v;
        puts("********** test list");
        {
            Timer _t("list build:");
            for (int i = 0; i < DATA_N; ++i) v.push_back(i);
        }
        {
            Timer _t("list action:");
            for (auto rn : rands) {
                if (rn % 2) {
                    int off = rn % (v.size() + 1);
                    auto iter = v.begin();
                    advance(iter, off);
                    v.insert(iter, rn);
                }
                else {
                    int off = rn % v.size();
                    auto iter = v.begin();
                    advance(iter, off);
                    v.erase(iter);
                }
            }
        }
        res2.assign(v.begin(), v.end());
    }
    cout << "result right:" << (res == res2) << endl;
#endif

#ifdef TEST_DEQUE
    {
        deque<int> v;
        puts("********** test deque");
        {
            Timer _t("deque build:");
            for (int i = 0; i < DATA_N; ++i) v.push_back(i);
        }
        {
            Timer _t("deque action:");
            for (auto rn : rands) {
                if (rn % 2) {
                    int off = rn % (v.size() + 1);
                    v.insert(v.begin() + off, rn);
                }
                else {
                    int off = rn % v.size();
                    v.erase(v.begin() + off);
                }
            }
        }
        res2.assign(v.begin(), v.end());
    }
    cout << "result right:" << (res == res2) << endl;
#endif

    {
        SkipListArray v(12);
        puts("********** test SkipListArray");
        {
            Timer _t("SkipListArray build:");
            for (int i = 0; i < DATA_N; ++i) v.insert(v.size(), i);
        }
        {
            Timer _t("SkipListArray action:");
            for (auto rn : rands) {
                if (rn % 2) {
                    int off = rn % (v.size() + 1);
                    v.insert(off, rn);
                }
                else {
                    int off = rn % v.size();
                    v.erase(off);
                }
            }
        }
        res2 = v.toList();
    }
    cout << "result right:" << (res == res2) << endl;
}

void functionTest()
{
    const int DATA_N = 1 << 10;
    const int RAND_N = 1 << 10;

    vector<int> v;
    SkipListArray sla(10);
    
    for (int i = 0; i < DATA_N; ++i) v.push_back(i);
    for (int i = 0; i < DATA_N; ++i) sla.insert(sla.size(), i);
    assert(v == sla.toList());

    for (int i = 0; i < RAND_N; ++i) {
        if (myrand() % 2) {
            int off = myrand() % v.size();
            v.erase(v.begin() + off);
            sla.erase(off);
        }
        else {
            int off = myrand() % (v.size() + 1);
            v.insert(v.begin() + off, off + i);
            sla.insert(off, off + i);
        }
    }
    assert(v == sla.toList());
    for (auto i = 0; i < (int)v.size(); ++i) assert(v[i] == sla[i]);
}

int main()
{
    srand((int)time(NULL));

    //_CrtSetDbgFlag( _CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF);    
    functionTest();
    performanceTest();
}
