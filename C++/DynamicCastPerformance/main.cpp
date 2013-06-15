#include "pch.h" 

#include <time.h>

// ==============================
struct MemberBase {
    virtual ~MemberBase(){}
    MemberBase(int typeID = TYPE_ID): m_typeID(typeID){}
    int m_typeID;
    static const int TYPE_ID = 1;
};
template<typename T> 
inline T* memberDynamicCast(MemberBase *p) {
    return T::isInstanceOf(p) ? static_cast<T*>(p) : NULL;
}
struct MemberDerived1 : public MemberBase {
    MemberDerived1(int typeID = TYPE_ID): MemberBase(typeID){}
    static const int TYPE_ID = 2;
    static bool isInstanceOf(MemberBase *p);
};
struct MemberDerived1_1 : public MemberDerived1 {
    MemberDerived1_1(int typeID = TYPE_ID): MemberDerived1(typeID){}
    static const int TYPE_ID = 3;
    static bool isInstanceOf(MemberBase *p);
};
struct MemberDerived2 : public MemberBase {
    MemberDerived2(int typeID = TYPE_ID): MemberBase(typeID){}
    static const int TYPE_ID = 4;
    static bool isInstanceOf(MemberBase *p);
};
bool MemberDerived1::isInstanceOf(MemberBase *p) {
    return p->m_typeID == MemberDerived1::TYPE_ID || p->m_typeID == MemberDerived1_1::TYPE_ID;
}
bool MemberDerived1_1::isInstanceOf(MemberBase *p) {
    return p->m_typeID == MemberDerived1_1::TYPE_ID;
}
bool MemberDerived2::isInstanceOf(MemberBase *p) {
    return p->m_typeID == MemberDerived2::TYPE_ID;
}
// ==============================
struct VirFuncBase {
    virtual ~VirFuncBase(){}
    static const int TYPE_ID = 1;
    virtual bool isInstanceOf(int typeID) {
        return TYPE_ID == typeID;
    }
};
template<typename T> 
inline T* virFuncDyanmicCast(VirFuncBase *p) {
    return p->isInstanceOf(T::TYPE_ID) ? static_cast<T*>(p) : NULL;
}
struct VirFuncDerived1 : public VirFuncBase {
    typedef VirFuncBase SuperT;
    static const int TYPE_ID = 2;
    virtual bool isInstanceOf(int typeID) {
        return TYPE_ID == typeID || SuperT::isInstanceOf(typeID);
    }
};
struct VirFuncDerived1_1 : public VirFuncDerived1 {
    typedef VirFuncDerived1 SuperT;
    static const int TYPE_ID = 3;
    virtual bool isInstanceOf(int typeID) {
        return TYPE_ID == typeID || SuperT::isInstanceOf(typeID);
    }
};
struct VirFuncDerived2 : public VirFuncBase {
    typedef VirFuncBase SuperT;
    static const int TYPE_ID = 4;
    virtual bool isInstanceOf(int typeID) {
        return TYPE_ID == typeID || SuperT::isInstanceOf(typeID);
    }
};
// ==============================
void testMemberBaseCast(MemberBase *p) {
    puts("********* begin testMemberBaseCast:");
    if (MemberDerived1 *_p = memberDynamicCast<MemberDerived1>(p)) puts("is MemberDerived1 ");
    if (MemberDerived1_1 *_p = memberDynamicCast<MemberDerived1_1>(p)) puts("is MemberDerived1_1 ");
    if (MemberDerived2 *_p = memberDynamicCast<MemberDerived2>(p)) puts("is MemberDerived2 ");
    puts("********* end test");
}
void testVirFuncCast(VirFuncBase *p) {
    puts("********* begin testVirFuncCast:");
    if (VirFuncDerived1* _p = virFuncDyanmicCast<VirFuncDerived1>(p)) puts("is VirFuncDerived1");
    if (VirFuncDerived1_1* _p = virFuncDyanmicCast<VirFuncDerived1_1>(p)) puts("is VirFuncDerived1_1");
    if (VirFuncDerived2* _p = virFuncDyanmicCast<VirFuncDerived2>(p)) puts("is VirFuncDerived2");
    puts("********* end test");
}

void functionalTest() {
    testMemberBaseCast(new MemberBase());
    testMemberBaseCast(new MemberDerived1());
    testMemberBaseCast(new MemberDerived1_1());
    testMemberBaseCast(new MemberDerived2());

    testVirFuncCast(new VirFuncBase());
    testVirFuncCast(new VirFuncDerived1());
    testVirFuncCast(new VirFuncDerived1_1());
    testVirFuncCast(new VirFuncDerived2());
}
// ==============================
int testMemberBasePerformance_buildin(MemberBase *p, int n) {
    int sum = 0;
    clock_t start = clock();
    for (int i = 0; i < n; ++i) {
        if (MemberDerived1 *_p = dynamic_cast<MemberDerived1*>(p)) sum += _p->TYPE_ID;
        if (MemberDerived1_1 *_p = dynamic_cast<MemberDerived1_1*>(p)) sum += _p->TYPE_ID;
        if (MemberDerived2 *_p = dynamic_cast<MemberDerived2*>(p)) sum += _p->TYPE_ID;
    }
    printf("testMemberBasePerformance buildin : %f\n", float(clock() - start) / CLOCKS_PER_SEC);
    return sum;
}
int testMemberBasePerformance(MemberBase *p, int n) {
    int sum = 0;
    clock_t start = clock();
    for (int i = 0; i < n; ++i) {
        if (MemberDerived1 *_p = memberDynamicCast<MemberDerived1>(p)) sum += _p->TYPE_ID;
        if (MemberDerived1_1 *_p = memberDynamicCast<MemberDerived1_1>(p)) sum += _p->TYPE_ID;
        if (MemberDerived2 *_p = memberDynamicCast<MemberDerived2>(p)) sum += _p->TYPE_ID;
    }
    printf("testMemberBasePerformance : %f\n", float(clock() - start) / CLOCKS_PER_SEC);
    return sum;
}
int testVirFuncBasePerformance_buildin(VirFuncBase *p, int n) {
    int sum = 0;
    clock_t start = clock();
    for (int i = 0; i < n; ++i) {
        if (VirFuncDerived1 *_p = dynamic_cast<VirFuncDerived1*>(p)) sum += _p->TYPE_ID;
        if (VirFuncDerived1_1 *_p = dynamic_cast<VirFuncDerived1_1*>(p)) sum += _p->TYPE_ID;
        if (VirFuncDerived2 *_p = dynamic_cast<VirFuncDerived2*>(p)) sum += _p->TYPE_ID;
    }
    printf("testVirFuncBasePerformance buildin : %f\n", float(clock() - start) / CLOCKS_PER_SEC);
    return sum;
}
int testVirFuncBasePerformance(VirFuncBase *p, int n) {
    int sum = 0;
    clock_t start = clock();
    for (int i = 0; i < n; ++i) {
        if (VirFuncDerived1 *_p = virFuncDyanmicCast<VirFuncDerived1>(p)) sum += _p->TYPE_ID;
        if (VirFuncDerived1_1 *_p = virFuncDyanmicCast<VirFuncDerived1_1>(p)) sum += _p->TYPE_ID;
        if (VirFuncDerived2 *_p = virFuncDyanmicCast<VirFuncDerived2>(p)) sum += _p->TYPE_ID;
    }
    printf("testVirFuncBasePerformance : %f\n", float(clock() - start) / CLOCKS_PER_SEC);
    return sum;
}

void performanceTest() {
    const int N = 1 << 24;

    testMemberBasePerformance_buildin(new MemberBase(), N);
    testMemberBasePerformance_buildin(new MemberDerived1(), N);
    testMemberBasePerformance_buildin(new MemberDerived1_1(), N);
    testMemberBasePerformance_buildin(new MemberDerived2(), N);
    testMemberBasePerformance(new MemberBase(), N);
    testMemberBasePerformance(new MemberDerived1(), N);
    testMemberBasePerformance(new MemberDerived1_1(), N);
    testMemberBasePerformance(new MemberDerived2(), N);

    testVirFuncBasePerformance_buildin(new VirFuncBase(), N);
    testVirFuncBasePerformance_buildin(new VirFuncDerived1(), N);
    testVirFuncBasePerformance_buildin(new VirFuncDerived1_1(), N);
    testVirFuncBasePerformance_buildin(new VirFuncDerived2(), N);
    testVirFuncBasePerformance(new VirFuncBase(), N);
    testVirFuncBasePerformance(new VirFuncDerived1(), N);
    testVirFuncBasePerformance(new VirFuncDerived1_1(), N);
    testVirFuncBasePerformance(new VirFuncDerived2(), N);
}
    
int main() {
    functionalTest();
    performanceTest();
}
