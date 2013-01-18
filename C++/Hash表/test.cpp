// vim:fileencoding=gbk

#include "pch.h"

#include <ctime>
#include <cassert>

#include "Hash.h"
#include <set>
#include <string>
#include <hash_set>
#include <vector>
#include <fstream>
#include <algorithm>

#pragma warning(disable: 4018)
#pragma warning(disable: 4267)

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

void performanceTest_string()
{
    const int N = 1;

    std::vector<std::string> textArray;
    {
        std::ifstream fi("1.txt");
        for (std::string s; getline(fi, s);) textArray.push_back(s);
    }

    std::vector<int> rArray;
    for (int i = 0; i < (1 << 20); ++i) rArray.push_back(rand() % textArray.size());

    std::vector<std::string> res;

    {
        Timer _t("HashSet");
        for (int _ = 0; _ < N; ++_) {
            HashSet<std::string> s;
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

            if (res.empty()) {
                res.assign(s.begin(), s.end());
            }
        }
    }

    std::sort(res.begin(), res.end());
    cout << res.size() << endl;

    std::vector<std::string> res2;

    {
        Timer _t("set");
        for (int _ = 0; _ < N; ++_) {
            std::set<std::string> s;
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
    cout << (res == res2) << endl;

    {
        Timer _t("hash_set");
        for (int _ = 0; _ < N; ++_) {
            stdext::hash_set<std::string> s;
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
    cout << (res == res2) << endl;
}

void performanceTest_int()
{
    const int N = 1;
    std::vector<int> textArray;
    {
        for (int i = 0; i < (1 << 18); ++i) textArray.push_back(rand() % 4096);
    }

    std::vector<int> rArray;
    for (int i = 0; i < (1 << 20); ++i) rArray.push_back(rand() % textArray.size());

    std::vector<int> res;

    {
        Timer _t("HashSet");
        for (int _ = 0; _ < N; ++_) {
            HashSet<int> s;
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

            if (res.empty()) {
                res.assign(s.begin(), s.end());
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
    cout << (res == res2) << endl;

    {
        Timer _t("hash_set");
        for (int _ = 0; _ < N; ++_) {
            stdext::hash_set<int> s;
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
    cout << (res == res2) << endl;
}

void syntaxTest_HashSet()
{
    {
        HashSet<int> s;
        s.insert(1);
        s.insert(2);
        s.insert(1);
        s.insert(3);
        s.insert(2);
        assert(s.size() == 3);
        s.erase(3);
        assert(s.size() == 2);
        s.erase(3);
        assert(s.size() == 2);
    }
    {
        int a[5] = {1, 3, 2, 4, 1};
        HashSet<int> s(a, a + 5);
        assert(s.size() == 4);
        {
            HashSet<int> s2(s);
            assert(s2.size() == 4);
        }
        {
            HashSet<int> s2;
            assert(s2.empty());
            s2 = s;
            assert(s2.size() == 4);
            assert(!s2.empty());
            assert(s2.contain(3));
            assert(s2.count(2) == 1);
            assert(s2 == s);
            assert(!(s2 != s));
        }
    }
    {
        std::vector<int> v(5, 0);
        HashSet<int> s;
        s.insert(v.begin(), v.end());
        assert(s.size() == 1);
        assert(s.find(1) == s.end());
        assert(s.find(0) != s.end());
        assert(s.find(0) == s.begin());
        s.erase(s.find(0));
        assert(s.empty());
        s.insert(5);
        assert(!s.empty());
        s.clear();
        assert(s.empty());

        HashSet<int> s2;
        s2.insert(5);
        std::swap(s, s2);
        assert(s2.empty());
        assert(s.find(5) != s.end());
    }
    {
        int a[4] = {1,2, 3, 1};
        const HashSet<int> s(a, a + 4);
        HashSet<int> b;
        for (HashSet<int>::ConstIterator iter = s.begin();
                iter != s.end();
                ++iter) {
            b.insert(*iter);
        }
        assert(b.size() == 3);
    }
}

void syntaxTest_HashMap()
{
    {
        std::pair<int, int> a[] = { std::make_pair(3, 4)};
        HashMap<int, int> m(a, a + 1);
        assert(m.size() == 1);
        assert(!m.empty());
        assert(!m.contain(2));
        assert(m.count(3) == 1);

        HashMap<int, int> m2;
        m2 = m;
        assert(m2.size() == 1);
        assert(m2 == m);
        assert(!(m2 != m));
        m2.erase(3);
        assert(m2.empty());
        assert(m.find(5) == m.end());
        m.erase(m.find(3));
        assert(m.empty());

        HashMap<int, int> m3(a, a + 1);
        assert(m3.size() == 1);
        std::swap(m, m3);
        assert(m3.empty());
        assert(m.size() == 1);
        assert(*m.begin() == std::make_pair(3, 4));
        assert(m.begin()->second == 4);
    }
    {
        HashMap<int, int> m;
        for (int i = 0; i < 100; ++i) {
            ++m[i % 10];
        }
        assert(m[0] == 10);
        assert(m[1] == 10);
    }
    {
        std::pair<int, int> a[] = {std::pair<int, int>(1, 2)};
        const HashMap<int, int> m(a, a + 1);
        assert(m.size() == 1);
        for (HashMap<int, int>::ConstIterator i = m.begin();
                i != m.end();
                ++i) assert(i->first == 1);
    }
}

int main()
{
    srand((unsigned int)time(NULL));

    syntaxTest_HashSet();
    syntaxTest_HashMap();
    performanceTest_string();
    performanceTest_int();
}
