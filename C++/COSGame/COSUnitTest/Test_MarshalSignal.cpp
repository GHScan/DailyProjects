#include "stdafx.h"

#include <boost/bind.hpp>

#include "../COSFoundationLib/MarshalSignal.h"
#include "../COSFoundationLib/Utility.h"

#include "Tests.h"

namespace Scan
{

namespace 
{
    struct MarshalSlot:
        public MarshalSignal::IMarshalSlot
    {
        virtual const String onEmit(const StringVec& params)
        {
            v = params;
            if (!v.empty() && v[0] == "1") return "true";
            return "false";
        }
        StringVec v;
    };
}

static void test_marshalSignal_marshalSignal()
{
    MarshalSignal sig;
    ASSERT(sig.isNull());
    ASSERT(sig.emit<int>() == 0);
    
    boost::shared_ptr<MarshalSlot>  slot(new MarshalSlot);
    sig.connect(slot);
    ASSERT(!sig.isNull());
    ASSERT(sig.getSlotCount() == 1);

    String r = sig.emit<String>(3, 1.25f, "呵呵");
    ASSERT(r == "false");
    ASSERT(slot->v.size() == 3 && slot->v[0] == "3" && slot->v[1] == "1.25" && slot->v[2] == "呵呵");

    bool b = sig.emit<bool>(1, String("中国人"));
    ASSERT(b);
    ASSERT(slot->v.size() == 2 && slot->v[0] == "1" && slot->v[1] == "中国人");

    boost::shared_ptr<MarshalSlot>  slot2(new MarshalSlot);
    sig.connect(slot2);
    ASSERT(sig.getSlotCount() == 2);
    ASSERT(sig.emit<bool>(1));
    ASSERT(slot->v == slot2->v && slot->v.size() == 1 && slot->v[0] == "1");

    slot->v.clear();
    slot2->v.clear();

    sig.disconnectAll();
    ASSERT(sig.isNull());
    ASSERT(sig.emit<String>(4).empty());
    ASSERT(slot->v == slot2->v && slot->v.empty());

    sig.connect(slot);
    sig.connect(slot2);
    sig.compact();
    ASSERT(sig.getSlotCount() == 2);
    slot.reset();
    slot2.reset();
    sig.compact();
    ASSERT(sig.getSlotCount() == 0);
}

static void test_marshalSignal_marshalSignalSlot()
{
    MarshalSignal sig;

    boost::shared_ptr<MarshalSlot>  slot(new MarshalSlot);
    MarshalSignal::Connection con = sig.connect(slot);
    ASSERT(con.connected());
    ASSERT(!con.blocked());

    ASSERT(sig.emit<bool>(1));
    ASSERT(slot->v.size() == 1);
    slot->v.clear();

    con.block();
    ASSERT(con.blocked());
    ASSERT(!sig.emit<bool>(1));
    ASSERT(slot->v.size() == 0);

    con.unblock();
    ASSERT(!con.blocked());
    ASSERT(sig.emit<bool>(1));
    ASSERT(slot->v.size() == 1);

    slot.reset();
    ASSERT(!con.connected());
    ASSERT(sig.getSlotCount() > 0);
    sig.compact();
    ASSERT(sig.isNull());

    slot.reset(new MarshalSlot);
    con = sig.connect(slot);
    ASSERT(sig.getSlotCount() == 1);
    ASSERT(con.connected());
    sig.disconnectAll();
    ASSERT(!con.connected());
}

int g_counter = 0;
static void addToCounter(const int& i)
{
    g_counter += i;
}

static void test_marshalSignal_unmarshalSignal()
{
    g_counter = 0;

    UnmarshSignal sig;
    sig.connect(addToCounter);

    struct UnmarshalSlot
    {
        void addCounter(const int &i)
        {
            g_counter += i;
        }
    }slot;
    sig.connect<void(const int&)>(boost::bind(&UnmarshalSlot::addCounter, &slot, _1));

    ASSERT(!sig.isNull());
    ASSERT(sig.emit(StringVec() << "5").empty());
    ASSERT(g_counter == 10);

    sig.disconnectAll();
    ASSERT(sig.isNull());
    
    struct UnmarshalSlot2
    {
        int addCounter(int i)
        {
            return g_counter += i * 2;
        }
    } slot2;
    sig.undefine();
    sig.connect<int(int)>(boost::bind(&UnmarshalSlot2::addCounter, &slot2, _1));
    ASSERT(sig.emit(StringVec() << "2") == "14");
    ASSERT(g_counter == 14);
}

static void addToCounter2(const int& i, const String& s)
{
    g_counter += i;
    int j;
    if (fromString(j, s)) g_counter += j;
}

static void test_marshalSignal_cooperation()
{
    MarshalSignal marshalSig;
    UnmarshSignal unmarshalSig;

    g_counter = 0;
    unmarshalSig.connect(addToCounter2);

    struct DummpyDeleter
    {
        void operator () (MarshalSignal::IMarshalSlot* p) const {}
    };
    // 避免删除栈数据
    boost::shared_ptr<UnmarshSignal> unmarshalSigPtr(&unmarshalSig, DummpyDeleter());
    marshalSig.connect(unmarshalSigPtr); 

    // 参数不足, 没影响
    marshalSig.emit<void>();
    ASSERT(g_counter == 0);

    ASSERT(marshalSig.emit<int>(5, "8") == 0);
    ASSERT(g_counter == 13);

    ASSERT(marshalSig.emit<int>(7.1f, 3, "多余参数忽略") == 0);
    ASSERT(g_counter == 23);

    ASSERT(marshalSig.emit<int>("a", "b", "多余参数忽略") == 0);
    ASSERT(g_counter == 23);
}

BEGIN_TEST(test_marshalSignal)
TEST(test_marshalSignal_marshalSignal)
TEST(test_marshalSignal_marshalSignalSlot)
TEST(test_marshalSignal_unmarshalSignal)
TEST(test_marshalSignal_cooperation)
END_TEST(test_marshalSignal)

}