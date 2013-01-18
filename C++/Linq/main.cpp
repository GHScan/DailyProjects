#include "pch.h" 

#include "linq.h"

#include <vector>
#include <string>
#include <algorithm>

template<typename T>
void printC(const T& v)
{
    for (auto i : v) cout << i << ',';
    cout << endl;
}

template<typename T>
void print(const T& v)
{
    cout << v << endl;
}

bool startsWith(const std::string& s, const std::string& prefix)
{
    return s.find(prefix) == 0;
}

void featureTest()
{
    // 1. standard
    {
        auto query = range(10)
            .where([](int i){ return i % 2; })
            .select([](int i){ return i + 1; });
        auto ref = {2, 4, 6, 8, 10};
        assert(std::equal(ref.begin(), ref.end(), query.begin()));
    }
    // 2. deferred range
    {
        assert(range(123LL, 1000000000000LL).first() == 123);
    }
    // 3. deferred action
    {
        int selectCnt = 0;

        auto query = range(1, 1000)
            .where([](int i){ return i % 2 == 0; })
            .select([&selectCnt](int i)
                { 
                    ++selectCnt;
                    return i; 
                })
            .where([](int i) { return i % 4 == 0; })
            .head(2);
        auto query2 = query;

        for (auto i : query) {}
        assert(selectCnt == 4);

        for (auto i : query2) {}
        assert(selectCnt == 8);
    }
    // 4. copy semantic
    {
        auto query = range(10).head(5);
        auto query2 = query;

        auto ref = {0, 1, 2, 3, 4};
        assert(std::equal(ref.begin(), ref.end(), query.begin()));
        assert(std::equal(ref.begin(), ref.end(), query2.begin()));

        auto iter = query.begin();
        ++iter;
        auto iter2 = iter;

        ref = {1, 2, 3, 4};
        assert(std::equal(ref.begin(), ref.end(), iter));
        assert(std::equal(ref.begin(), ref.end(), iter2));
    }
    // 5. always reference the neweast data of dataSource
    {
        std::vector<std::string> dataSrc{"A_abc", "A_def", "B_abc", "B_def"};

        auto query = from(dataSrc)
            .head(3)
            .where([](const std::string& s) { return startsWith(s, "B_"); });

        auto ref = {"B_abc"};
        assert(std::equal(ref.begin(), ref.end(), query.begin()));

        dataSrc.clear();
        dataSrc.shrink_to_fit();
        dataSrc = {"B#_abc", "B_123", "B_1234", "B_321", "B_111"};
        ref = {"B_123", "B_1234"};
        assert(std::equal(ref.begin(), ref.end(), query.begin()));
    }
    // 6. invoke the operator new as least as possible
    {
    }
    // 7. you can use query after the dataSource has been destroyed, by the use of 'reserve'
    {
        Enumerable<int> query;
        {
            std::vector<int> v{1, 2, 3, 4};
            // query = from(v).select([](int i){ return i % 2; });
            query = from(v).reserve().select([](int i){ return i % 2; });

            auto ref = {1, 0, 1, 0};
            assert(std::equal(ref.begin(), ref.end(), query.begin()));
        }

        auto ref = {1, 0, 1, 0};
        assert(std::equal(ref.begin(), ref.end(), query.begin()));
    }
    // 8. add action to an exist query
    {
        auto query = range(10).where([](int i){ return i < 5;});
        auto ref = {0, 1, 2, 3, 4};
        assert(std::equal(ref.begin(), ref.end(), query.begin()));

        auto query2 = query.select([](int i){ return i * i; });
        ref = {0, 1, 4, 9, 16};
        assert(std::equal(ref.begin(), ref.end(), query2.begin()));
    }
}

void functionTest()
{
    // 1. from, select, where, cast
    {
        int a[]{5, 6, 7, 8, 9};
        auto query = from(a)
            .where([](int i){ return i % 3; })
            .select([](int i) { return i * i;});
        auto ref = {25, 49, 64};
        assert(std::equal(ref.begin(), ref.end(), query.begin()));
    }
    // 2. range, all, any
    {
        assert(range(10).all([](int i){ return i >= 0;}));
        assert(!range(10).all([](int i){ return i > 0;}));
        assert(from(std::vector<std::string>{"_a", "b"})
                .any([](const std::string& s){ return startsWith(s, "_"); }));
        assert(!from(std::vector<std::string>{"@a", "b"})
                .any([](const std::string& s){ return startsWith(s, "_"); }));
    }
    // 3. cast, average
    {
        assert(range(1, 5).average() == 2);
        assert(range(1, 5).cast<float>().average() == 2.5);
    }
    // 4. contain, count
    {
        int a[]{1, 2, 1, 1, 3, 2, };
        assert(from(a).contain(3));
        assert(!from(a).contain(4));
        assert(from(a).count(1) == 3);
        assert(from(a).count([](int i) { return i % 2; }) == 4);
    }
    // 5. first, last, head, tail
    {
        int a[]{3, 5, 7, 9, 11};
        assert(from(a).first() == 3);
        assert(from(a).last() == 11);

        auto ref = {3, 5};
        auto query = from(a).head(2);
        assert(std::equal(ref.begin(), ref.end(), query.begin()));

        ref = {7, 9, 11};
        query = from(a).tail(3);
        assert(std::equal(ref.begin(), ref.end(), query.begin()));
    }
    // 6. groupBy
    {
        auto query = range(10).groupBy([](int i) { return i % 3;});
        int refs[][4] = {
            {0, 3, 6, 9},
            {1, 4, 7,},
            {2, 5, 8,},
        };
        int n = 0;
        for (auto i : query) {
            assert(i.first == refs[n][0]);
            assert(std::equal(i.second.begin(), i.second.end(), refs[n]));
            ++n;
        }
        assert(n == 3);
    }
    // 7. takeUntil, skipUntil
    {
        auto query = range(10).takeUntil([](int i){ return i > 5; });
        auto ref = {0, 1, 2, 3, 4, 5};
        assert(std::equal(ref.begin(), ref.end(), query.begin()));

        query = range(10).skipUntil([](int i){ return i > 5; });
        ref = { 6, 7, 8, 9};
        assert(std::equal(ref.begin(), ref.end(), query.begin()));
    }
    // 8. max, min
    {
        int a[]{3, 2, 5, 8, 10, -3};
        assert(from(a).min() == -3);
        assert(from(a).max() == 10);
    }
    // 9. reduce
    {
        assert(range(1, 11).reduce([](int a, int b){ return a + b; }) == 55);
        assert(range(1, 11).reduce([](int a, int b){ return a * b; }, 0) == 0);
        assert(range(1, 11).reduce([](int a, int b){ return a * b; }, 1) == 3628800);
    }
    // 10. unique, sort, random
    {
        int a[]{3, 5, 5, 4, 2, 1, 2};

        auto query = from(a).unique();
        auto ref = {3, 5, 4, 2, 1};
        assert(std::equal(ref.begin(), ref.end(), query.begin()));

        ref = {5, 4, 3, 2, 1};
        query = query.sort().sort([](int a, int b){ return a > b; });
        assert(std::equal(ref.begin(), ref.end(), query.begin()));
        query = query.random();
        assert(!std::equal(ref.begin(), ref.end(), query.begin()));
    }
    // 11. intersect, _union
    {
        int a[]{3, 5, 11};
        auto query = range(10).intersect(from(a));
        auto ref = {3, 5};
        assert(std::equal(ref.begin(), ref.end(), query.begin()));

        ref = {3, 4, 5, 6};
        query = query._union(range(4, 7));
        assert(std::equal(ref.begin(), ref.end(), query.begin()));
    }
}

int main()
{
    featureTest();
    functionTest();
}
