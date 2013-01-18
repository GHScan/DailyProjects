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

    // ������ȷ
    ASSERT(p.isTypeOf<int>());

    // ���Ͷ�������޸�
    // p.getPtr<float>();

    // ֵ��ͬ
    ASSERT(p.getPtr<int>().get() == i);

    // ����乲������
    Property p2 = p;
    ASSERT(p2.getPtr<int>().get() == i);

    // ȡ������
    p2.undefine();
    ASSERT(p2.isNull());

    // ��������
    ASSERT(!p.isNull());

    // ���������ɹ�
    p2.define<long long>();
    ASSERT(p2.isTypeOf<long long>());
    ASSERT(!p2.isNull());
    ASSERT((void*)p2.getPtr<long long>().get() != p.getPtr<int>().get());

    const Property p3 = p;
    // ����
    // int *_k = p3.getPtr<int>().get();
    // ��ȷ
    const int *k = p3.getPtr<int>().get();

    // ����rawptr
    {
        p.getRawPtr<int>();

        long long *l;
        p2.getRawPtr(&l);
        ASSERT(p2.getPtr<long long>().get() == l);

        p3.getRawPtr<int>();
    }

    // ���Թ���
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
    // string�����л�
    {
        Property p;
        int *i = p.getRawPtr<int>();

        Property p2 = p;
        // ���ϸ������
        ASSERT(!p2.fromString("f15"));

        // �ϸ������
        ASSERT(p2.fromString("15"));
        // ����
        ASSERT(*i == 15);

        // ת�����ַ���
        ASSERT(p2.toString() == "15");
    }

    // ���������л�
    {
        // ��������
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

        // ��pod����
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