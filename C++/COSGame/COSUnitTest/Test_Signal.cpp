#include "stdafx.h"

#include "../COSFoundationLib/Utility.h"
#include "../COSFoundationLib/Signal.h"

#include "Tests.h"

namespace Scan
{

static int g_sigData = 0;
static void onIncSigData() 
{
    ++g_sigData;
}

static int onAdd(int i, float f)
{
    return int(i + f);
}
static int onAddSigData(int i, float f)
{
    g_sigData += int(i + f);
    return 1;
}
static float onRet5(int) { return 5; }

static void test_signal_compatible()
{
    // 测试空信号
    Signal void0;
    void0.connect(&onIncSigData);

    struct A
    {
        void onIncSigData() { ++g_sigData; }
    } a;
    void0.connect<void(void)>(boost::bind(&A::onIncSigData, &a));

    // 已经定义
    ASSERT(!void0.isNull());
    void0.emit<void>();
    ASSERT(g_sigData == 2);

    Signal int2 = void0;
    // 共享
    int2.emit<void>();
    ASSERT(g_sigData == 4);

    int2.undefine();
    int2.define<int(int, float)>();

    // 两个数据的
    int2.connect(onAdd);
    ASSERT((5 == int2.emit<int>(3, 2.14f)));

    int2.connect(onAddSigData);
    // singal的result取last_value
    ASSERT((int2.emit<int>(14, 12.f) == 1));
    ASSERT(g_sigData == 30);

    Signal float1;

    struct B
    {
        float onSubSigData(int d) { g_sigData -= d; return 3.14f; }
    }b;
    Signal::Connection c = float1.connect<float(int)>(boost::bind(&B::onSubSigData, b, _1));
    ASSERT(float1.emit<float>(14) == 3.14f && g_sigData == 16);

    float1.connect(onRet5, Signal::CP_Front);
    c.block(true);
    ASSERT(float1.emit<float>(1) == 5 && g_sigData == 16);
    c.unblock();

    ASSERT(float1.emit<float>(1) == 3.14f && g_sigData == 15);
}

BEGIN_TEST(test_signal)
TEST(test_signal_compatible)
END_TEST(test_signal)

}