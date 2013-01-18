#include "StdAfx.h"

#include <ctime>

#include <boost/progress.hpp>

#include "CopyBits.h"

#ifdef UNIT_TEST

namespace 
{

class Test_CopyBits
{
public:
    Test_CopyBits()
    {
        srand((uint)time(NULL));

        cout << "Test_CopyBits begin: " << endl;
        test_correct("copyBits_slow", copyBits_slow);
        test_correct("copyBits_quick", copyBits_quick);
        test_performance("copyBits_slow", copyBits_slow);
        test_performance("copyBits_quick", copyBits_quick);
        cout << "Test_CopyBits end: " << endl;
    }

private:
    typedef 
        void (*FuncT_CopyBits)(void *, size_t, const void *, size_t , size_t );

    void test_correct(const char *name, FuncT_CopyBits f)
    {
#ifdef _DEBUG
        const size_t TEST_TURN = 1 << 3;
#else
        const size_t TEST_TURN = 1 << 7;
#endif
        const size_t ARRAY_LEN = 1 << 16;
        byte src[ARRAY_LEN] = {0};
        byte dest[ARRAY_LEN] = {0};

        cout << "correction check : " << name << endl;

        size_t alignOffset = 0; // 用这个让源和目标不对齐
        for (size_t turn = 0; turn < TEST_TURN; ++turn)
        {
            for (size_t i = 0; i < ARRAY_LEN - 1; ++i)
            {
                src[i] = (byte)(rand() % 256);
            }

            size_t offset = 0;
            size_t maxLen = (sizeof(src) - 1) * 8;
            while (offset < maxLen)
            {
                size_t bitsToCopy = min<size_t>(rand() % 1024 + 1, maxLen - offset);
                f(dest, offset + alignOffset, src, offset, bitsToCopy);
                offset += bitsToCopy;
            }

            for (size_t i = 0; i < ARRAY_LEN - 1; ++i) 
            { // 恢复对齐
                byte desti = dest[i], destn = dest[i + 1];
                dest[i] = (byte)((desti << alignOffset) | (destn >> (8 - alignOffset)));
                assert(dest[i] == src[i]);
                if (dest[i] != src[i]) { cout << "incorrect !" << endl; return; }
            }

            ++alignOffset; alignOffset &= 0x7;
        }

        cout << "correct!" << endl;
    }

    void test_performance(const char *name, FuncT_CopyBits f)
    {
#ifdef _DEBUG 
        const size_t TEST_TURN = 1 << 4;
#else
        const size_t TEST_TURN = 1 << 10;
#endif
        const size_t ARRAY_LEN = 1 << 16;

        byte src[ARRAY_LEN] = {0};
        for (size_t i = 0; i < ARRAY_LEN - 1; ++i)
        {
            src[i] = (byte)(rand() % 256);
        }

        byte dest[ARRAY_LEN] = {0};

        cout << "performance test : " << name << endl;
        const char *testCases_name[] = {"align", "unalign"};
        size_t testCases_mul[] = {0, 1};

        for (size_t i = 0; i < sizeof(testCases_name) / sizeof(testCases_name[0]); ++i)
        {
            cout << "case " << testCases_name[i] << endl;
            boost::progress_timer _timer;

            size_t alignOffset = 0; // 用这个让源和目标不对齐
            for (size_t turn = 0; turn < TEST_TURN; ++turn)
            {
                size_t offset = 0;
                size_t maxLen = (sizeof(src) - 1) * 8;
                size_t len = 1;
                while (offset < maxLen)
                {
                    size_t bitsToCopy = min(++len, maxLen - offset);
                    f(dest, offset + alignOffset * testCases_mul[i], src, offset, bitsToCopy);
                    offset += bitsToCopy;
                }

                ++alignOffset; alignOffset &= 0x7;
            }
        }
    }
}g_copyBits;


} // namespace

#endif