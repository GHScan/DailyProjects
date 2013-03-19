
#include "pch.h"

#include <time.h>
#include <assert.h>

#include <unordered_set>
#include <set>

#include "SkipList_Int2.h"

#include "SkipList_Int2_v2.h"

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

void performanceTest_int()
{
    const int N = 1;
    const int QUERY_N = 4;
    std::vector<int> textArray;
    {
        for (int i = 0; i < (1 << 20); ++i) textArray.push_back(rand() % (1<<14));
    }

    std::vector<int> rArray;
    for (int i = 0; i < (1 << 20); ++i) rArray.push_back(rand() % textArray.size());

    std::vector<int> res;

    {
        Timer _t("SkipList_Int2");
        for (int _ = 0; _ < N; ++_) {
            SkipList_Int2 s;
            for (int i = 0; i < (int)textArray.size(); ++i) s.set(textArray[i], 5);
            for (int i = 0; i < (int)rArray.size(); ++i) {
                s.erase(textArray[rArray[i]]);
            }
            for (int i = 0; i < (int)rArray.size(); ++i) {
                if (i % 3 == 0) {
                    s.erase(textArray[rArray[i]]);
                }
                else {
                    s.set(textArray[rArray[i]], 3);
                }
            }

            {
                Timer _tquery("SkipList_Int2 query");
                int sum = 0;
                for (int i = 0; i < QUERY_N; ++i) {
                    for (auto j : textArray) sum += s.get(j);
                }
                cout << "end query:" << sum << endl;
            }

            if (res.empty()) {
                for (auto p : s.toList()) {
                    res.push_back(p.first);
                }
            }
        }
    }

    std::sort(res.begin(), res.end());
    cout << res.size() << endl;

    std::vector<int> res2;

    {
        Timer _t("set");
        for (int _ = 0; _ < N; ++_) {
            std::set<int> s;
            for (int i = 0; i < (int)textArray.size(); ++i) s.insert(textArray[i]);
            for (int i = 0; i < (int)rArray.size(); ++i) {
                s.erase(textArray[rArray[i]]);
            }
            for (int i = 0; i < (int)rArray.size(); ++i) {
                if (i % 3 == 0) {
                    s.erase(textArray[rArray[i]]);
                }
                else {
                    s.insert(textArray[rArray[i]]);
                }
            }

            {
                int sum = 0;
                Timer _tquery("set query");
                for (int i = 0; i < QUERY_N; ++i) {
                    for (auto j : textArray) sum += s.count(j);
                }
                cout << "end query:" << sum << endl;
            }

            if (res2.empty()) {
                res2.assign(s.begin(), s.end());
            }
        }
    }

    std::sort(res2.begin(), res2.end());
    cout << res2.size() << "," << (res == res2) << endl;

    {
        Timer _t("SkipList_Int2_v2 ");
        for (int _ = 0; _ < N; ++_) {
            SkipList_Int2_v2 s(12);
            for (int i = 0; i < (int)textArray.size(); ++i) s.set(textArray[i], i);
            for (int i = 0; i < (int)rArray.size(); ++i) {
                s.erase(textArray[rArray[i]]);
            }
            for (int i = 0; i < (int)rArray.size(); ++i) {
                if (rArray[i] & 3) {
                    s.erase(textArray[rArray[i]]);
                }
                else {
                    s.set(textArray[rArray[i]], i);
                }
            }

            {
                int sum = 0;
                Timer _tquery("SkipList_Int2_v2 query");
                for (int i = 0; i < QUERY_N; ++i) {
                    for (auto j : textArray) sum += s.get(j);
                }
                cout << "end query:" << sum << endl;
            }

            if (res2.empty()) {
                for (auto p : s.toList()) {
                    res2.push_back(p.first);
                }
            }
        }
    }

    std::sort(res2.begin(), res2.end());
    cout << res2.size() << "," << (res == res2) << endl;

    {
        Timer _t("hash_set");
        for (int _ = 0; _ < N; ++_) {
            std::unordered_set<int> s;
            for (int i = 0; i < (int)textArray.size(); ++i) s.insert(textArray[i]);
            for (int i = 0; i < (int)rArray.size(); ++i) {
                s.erase(textArray[rArray[i]]);
            }
            for (int i = 0; i < (int)rArray.size(); ++i) {
                if (rArray[i] & 3) {
                    s.erase(textArray[rArray[i]]);
                }
                else {
                    s.insert(textArray[rArray[i]]);
                }
            }

            {
                int sum = 0;
                Timer _tquery("hash_set query");
                for (int i = 0; i < QUERY_N; ++i) {
                    for (auto j : textArray) sum += s.count(j);
                }
                cout << "end query:" << sum << endl;
            }

            if (res2.empty()) {
                res2.assign(s.begin(), s.end());
            }
        }
    }

    std::sort(res2.begin(), res2.end());
    cout << res2.size() << "," << (res == res2) << endl;
}

void functionTest()
{
    vector<int> ranInts;
    for (int i = 0; i < (1<<15); ++i) ranInts.push_back(i % (1<<10));
    random_shuffle(ranInts.begin(), ranInts.end());

    SkipList_Int2_v2 list(10);
    set<int> set;
    for (int i = 0; i < (int)ranInts.size(); ++i) {
        int v = ranInts[i];
        switch (i % 3) {
            case 0:
                set.insert(v);
                list.set(v, 1);
                break;
            case 1:
                assert((set.count(v) == 1) == (list.get(v, 2) == 1));
                break;
            case 2:
                set.erase(v);
                list.erase(v);
                break;
            default:
                break;
        }
    }
}

int main()
{
    srand((int)time(NULL));

    //_CrtSetDbgFlag( _CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF);    
    functionTest();
    performanceTest_int();
}
