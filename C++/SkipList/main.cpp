
#include "pch.h"

#include <time.h>

#include <unordered_set>
#include <set>

#include "SkipList_Int2.h"

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
    std::vector<int> textArray;
    {
        for (int i = 0; i < (1 << 15); ++i) textArray.push_back(rand() % 4096);
    }

    std::vector<int> rArray;
    for (int i = 0; i < (1 << 20); ++i) rArray.push_back(rand() % textArray.size());

    std::vector<int> res;

    {
        Timer _t("SkipList_Int2");
        for (int _ = 0; _ < N; ++_) {
            SkipList_Int2 s;
            for (int i = 0; i < textArray.size(); ++i) s.set(textArray[i], 5);
            for (int i = 0; i < rArray.size(); ++i) {
                s.erase(textArray[rArray[i]]);
            }
            for (int i = 0; i < rArray.size(); ++i) {
                if (i % 3 == 0) {
                    s.erase(textArray[rArray[i]]);
                }
                else {
                    s.set(textArray[rArray[i]], 3);
                }
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

    return;

    std::vector<int> res2;

    {
        Timer _t("set");
        for (int _ = 0; _ < N; ++_) {
            std::set<int> s;
            for (int i = 0; i < textArray.size(); ++i) s.insert(textArray[i]);
            for (int i = 0; i < rArray.size(); ++i) {
                s.erase(textArray[rArray[i]]);
            }
            for (int i = 0; i < rArray.size(); ++i) {
                if (i % 3 == 0) {
                    s.erase(textArray[rArray[i]]);
                }
                else {
                    s.insert(textArray[rArray[i]]);
                }
            }

            if (res2.empty()) {
                res2.assign(s.begin(), s.end());
            }
        }
    }

    std::sort(res2.begin(), res2.end());
    cout << res2.size() << "," << (res == res2) << endl;

    {
        Timer _t("hash_set");
        for (int _ = 0; _ < N; ++_) {
            std::unordered_set<int> s;
            for (int i = 0; i < textArray.size(); ++i) s.insert(textArray[i]);
            for (int i = 0; i < rArray.size(); ++i) {
                s.erase(textArray[rArray[i]]);
            }
            for (int i = 0; i < rArray.size(); ++i) {
                if (rArray[i] & 3) {
                    s.erase(textArray[rArray[i]]);
                }
                else {
                    s.insert(textArray[rArray[i]]);
                }
            }
            if (res2.empty()) {
                res2.assign(s.begin(), s.end());
            }
        }
    }

    std::sort(res2.begin(), res2.end());
    cout << res2.size() << "," << (res == res2) << endl;
}

int main()
{
    performanceTest_int();
}
