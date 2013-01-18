#include "stdafx.h"

#include  <boost/bind.hpp>

#include "../COSFoundationLib/Utility.h"
#include "../COSFoundationLib/Property.h"

#include "Tests.h"

namespace Scan
{

static void test_property_access()
{
    Property p;

    int *i = p.getPtr<int>().get();

    // 类型正确
    ASSERT(p.isTypeOf<int>());

    // 类型定义后不能修改
    // p.getPtr<float>();

    // 值相同
    ASSERT(p.getPtr<int>().get() == i);

    // 对象间共享数据
    Property p2 = p;
    ASSERT(p2.getPtr<int>().get() == i);

    // 取消声明
    p2.undefine();
    ASSERT(p2.isNull());

    // 还有声明
    ASSERT(!p.isNull());

    // 重新声明成功
    p2.define<long long>();
    ASSERT(p2.isTypeOf<long long>());
    ASSERT(!p2.isNull());
    ASSERT((void*)p2.getPtr<long long>().get() != p.getPtr<int>().get());

    const Property p3 = p;
    // 错误
    // int *_k = p3.getPtr<int>().get();
    // 正确
    const int *k = p3.getPtr<int>().get();

    // 测试rawptr
    {
        p.getRawPtr<int>();

        long long *l;
        p2.getRawPtr(&l);
        ASSERT(p2.getPtr<long long>().get() == l);

        p3.getRawPtr<int>();
    }

    // 测试构造
    {
        Property p(5);
        ASSERT(p.get<int>() == 5);
    }

    ASSERT(p.get<int>() == *p.getRawPtr<int>());

    {
        int i = 0;
        p.get(&i);
        ASSERT(i == *p.getRawPtr<int>());

        p.set(4);
        ASSERT(p.get<int>() == 4);
    }
}

static void test_property_valueChangedSignal()
{
    Property p;

    static int ls_i = 0;
    struct _Slot
    {
        void onValueChanged (Property& p)
        {
            p.get(&ls_i);
        }
    } slot;
    p.valueChanged().connect(boost::bind(&_Slot::onValueChanged, &slot, _1));
    p.set(173);
    ASSERT(ls_i == 173);
}

static void test_property_serialize()
{
    // string的序列化
    {
        Property p;
        int *i = p.getRawPtr<int>();

        Property p2 = p;
        // 不合格的数据
        ASSERT(!p2.fromString("f15"));

        // 合格的数据
        ASSERT(p2.fromString("15"));
        // 解析
        ASSERT(*i == 15);

        // 转化成字符串
        ASSERT(p2.toString() == "15");
    }

    // 二进制序列化
    {
        // 基本类型
        Property p;
        p.define<int>();
        p.fromString("18");

        int i = 0;
        p.toBytes(&i, sizeof(i));
        ASSERT(i == 18);

        i = 32;
        p.fromBytes(&i, sizeof(i));
        ASSERT(p.toString() == "32");

        // pod
        {
            struct UserType
            {
                int c;
            }ut = {5};
            p.undefine();
            p.define<UserType>();

            p.fromBytes(&ut, sizeof(ut));
            ASSERT(p.get<UserType>().c == 5);
            p.getRawPtr<UserType>()->c = 13;
            p.toBytes(&ut, sizeof(ut));
            ASSERT(ut.c == 13);
        }

        // 非pod报错
        {
            /*struct UserType
            {
                UserType(){}
                int c;
            }ut; ut.c = 5;
            p.undefine();
            p.define<UserType>();

            p.fromBytes(&ut, sizeof(ut));
            ASSERT(p.get<UserType>().c == 5);
            p.getRawPtr<UserType>()->c = 13;
            p.toBytes(&ut, sizeof(ut));
            ASSERT(ut.c == 13);*/
        }
    }
}

BEGIN_TEST(test_property)
TEST(test_property_access)
TEST(test_property_valueChangedSignal)
TEST(test_property_serialize)
END_TEST(test_property)

}