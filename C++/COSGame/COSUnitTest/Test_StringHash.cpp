#include "stdafx.h"

#include <set>
#include <hash_set>

#include <boost/unordered_set.hpp>

#include <windows.h>

#include "../COSFoundationLib/Utility.h"
#include "../COSFoundationLib/StringHash.h"

#include "Tests.h"

namespace Scan
{

static LARGE_INTEGER g_timerBeginTick;
static void beginTimer()
{
    QueryPerformanceCounter(&g_timerBeginTick);
}

static size_t endTimer()
{
    LARGE_INTEGER end = {0};
    QueryPerformanceCounter(&end);
    return size_t(end.QuadPart - g_timerBeginTick.QuadPart);
}

static char* generateString()
{
    static const char ls_charTable[] = "0123456789abcdefghijklmnopqrstuvwxyz~!@#$%^&*()_||<>?,./";
    static char ls_buf[256] = {0};

    int len = max(rand() % sizeof(ls_buf), 1);
    ls_buf[len] = 0;
    while (--len >= 0)
        ls_buf[len] = ls_charTable[rand() % sizeof(ls_charTable)];
    return ls_buf;
}

template<typename SetT>
static size_t test_set(const std::vector<String>& strSrc, size_t loopCnt, size_t sameCnt)
{
    SetT st;
    for (size_t i = 0; i < strSrc.size(); ++i) st.insert(SetT::value_type(strSrc[i]));

    beginTimer();

    for (size_t i = 0; i < loopCnt; ++i)
    {
        SetT::value_type v(strSrc[rand() % strSrc.size()]);
        for (size_t j = 0; j < sameCnt; ++j)
        {
            st.find(v);
        }
    }

    return endTimer();
}


static void test_stringHash_compatible()
{
    stdext::hash_set<StringHash> st0;
    st0.insert(StringHash("012"));
    st0.insert(StringHash("123"));
    st0.insert(StringHash("abc"));
    boost::unordered_set<StringHash> st1;
    st1.insert(StringHash("234"));
    st1.insert(StringHash("345"));
    st1.insert(StringHash("abc"));

    {
        StringHash s("012");
        ASSERT(st0.count(s) == 1);
        ASSERT(st1.count(s) == 0);
    }

    {
        StringHash s("234");
        ASSERT(st0.count(s) == 0);
        ASSERT(st1.count(s) == 1);

        s.setString("abc");
        ASSERT(st0.count(s) == 1);
        ASSERT(st1.count(s) == 1);
    }
}

static void test_stringHash_performance()
{
    std::vector<String> strSrc;
    for (int i = 0; i < 1000; ++i) 
        strSrc.push_back(generateString());

#ifdef _DEBUG
    const size_t loopCnt = 1 << 9;
    const size_t sameCnt = 1 << 2;
#else
    const size_t loopCnt = 1 << 13;
    const size_t sameCnt = 1 << 4;
#endif

    {
        size_t tick0 = test_set<std::set<String>>(strSrc, loopCnt, 1);
        size_t tick1 = test_set<std::set<StringHash>>(strSrc, loopCnt, 1);
        float speedUp = float(tick0) /tick1;
        // 测试不至于减速太多
        ASSERT(speedUp > 0.5f);

        size_t tick2 = test_set<std::set<String>>(strSrc, loopCnt, sameCnt);
        size_t tick3 = test_set<std::set<StringHash>>(strSrc, loopCnt, sameCnt);
        float speedUp1 = float(tick2) /tick3;
        ASSERT(speedUp1 > 0.5f);
    }

    {
        size_t tick0 = test_set<stdext::hash_set<String>>(strSrc, loopCnt, 1);
        size_t tick1 = test_set<stdext::hash_set<StringHash>>(strSrc, loopCnt, 1);
        float speedUp = float(tick0) /tick1;
        ASSERT(speedUp > 0.5f);

        size_t tick2 = test_set<stdext::hash_set<String>>(strSrc, loopCnt, sameCnt);
        size_t tick3 = test_set<stdext::hash_set<StringHash>>(strSrc, loopCnt, sameCnt);
        float speedUp1 = float(tick2) /tick3;
        // 对于hash容器, 应该有加速作用
        ASSERT(speedUp1 > 1);
    }

    {
        size_t tick0 = test_set<boost::unordered_set<String>>(strSrc, loopCnt, 1);
        size_t tick1 = test_set<boost::unordered_set<StringHash>>(strSrc, loopCnt, 1);
        float speedUp = float(tick0) /tick1;
        ASSERT(speedUp > 0.5f);

        size_t tick2 = test_set<boost::unordered_set<String>>(strSrc, loopCnt, sameCnt);
        size_t tick3 = test_set<boost::unordered_set<StringHash>>(strSrc, loopCnt, sameCnt);
        float speedUp1 = float(tick2) /tick3;
        // 对于hash容器, 应该有加速作用
        ASSERT(speedUp1 > 1);
    }
}

BEGIN_TEST(test_stringHash)
TEST(test_stringHash_compatible)
TEST(test_stringHash_performance)
END_TEST(test_stringHash)

}