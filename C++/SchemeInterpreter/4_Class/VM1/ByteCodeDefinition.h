#ifndef BYTECODEDEFINITION_H
#define BYTECODEDEFINITION_H

struct SClassProto;
struct SFuncProto;

enum ByteCodeEnum {
    BCE_LoadBool,
    BCE_LoadInt,
    BCE_LoadDouble,
    BCE_LoadConstant,

    BCE_LoadGlobal,
    BCE_StoreGlobal,

    BCE_LoadLocal,
    BCE_StoreLocal,

    BCE_LoadFree,
    BCE_LoadFree1,
    BCE_LoadFree2,
    BCE_LoadFree3,
    BCE_StoreFree,
    BCE_StoreFree1,
    BCE_StoreFree2,
    BCE_StoreFree3,

    BCE_LoadFunc,

    BCE_Pop,

    BCE_Jmp,
    BCE_TrueJmp,

    BCE_Tail,

    BCE_Call,

    BCE_LoadClass,

    BCE_LoadMethod,
    // this can be infered at compile time... here use a cache to simplify the implemention of assembler
    BCE_LoadCachedMethod, 

    BCE_GetField,
    BCE_GetCachedField,

    BCE_GetMethod,
    BCE_GetCachedMethod,

    BCE_Inline_Add,
    BCE_Inline_Sub,
    BCE_Inline_Mul,
    BCE_Inline_Div,
    BCE_Inline_Quotient,
    BCE_Inline_Mod,
    BCE_Inline_Num_Equal,
    BCE_Inline_Num_Less,
    BCE_Inline_Num_LessEq,
    BCE_Inline_Num_Greater,
    BCE_Inline_Num_GreaterEq,
    BCE_Inline_Not,
    BCE_Inline_Eq,
    BCE_Inline_Empty,
    BCE_Inline_Cons,
    BCE_Inline_Car,
    BCE_Inline_Cdr,
};

#pragma pack(push, 1)

template<int>
struct ByteCode;

template<>
struct ByteCode<BCE_LoadBool> {
    uint8_t code;
    uint8_t value;

    explicit ByteCode(int i): code(BCE_LoadBool) {
        checkedAssign(&value, i);
    }
};

template<>
struct ByteCode<BCE_LoadInt> {
    uint8_t code;
    int value;

    explicit ByteCode(int i): code(BCE_LoadInt) {
        value  = i;
    }
};

template<>
struct ByteCode<BCE_LoadDouble> {
    uint8_t code;
    double value;

    explicit ByteCode(double i): code(BCE_LoadDouble) {
        value = i;
    }
};

template<>
struct ByteCode<BCE_LoadConstant> {
    uint8_t code;
    uint16_t kindex;

    explicit ByteCode(int i): code(BCE_LoadConstant) {
        checkedAssign(&kindex, i);
    }
};

template<>
struct ByteCode<BCE_LoadGlobal> {
    uint8_t code;
    uint16_t gindex;

    explicit ByteCode(int i): code(BCE_LoadGlobal) {
        checkedAssign(&gindex, i);
    }
};

template<>
struct ByteCode<BCE_StoreGlobal> {
    uint8_t code;
    uint16_t gindex;

    explicit ByteCode(int i): code(BCE_StoreGlobal) {
        checkedAssign(&gindex, i);
    }
};

template<>
struct ByteCode<BCE_LoadLocal> {
    uint8_t code;
    uint8_t lindex;

    explicit ByteCode(int i): code(BCE_LoadLocal) {
        checkedAssign(&lindex, i);
    }
};

template<>
struct ByteCode<BCE_StoreLocal> {
    uint8_t code;
    uint8_t lindex;

    explicit ByteCode(int i): code(BCE_StoreLocal) {
        checkedAssign(&lindex, i);
    }
};

template<>
struct ByteCode<BCE_LoadFree> {
    uint8_t code;
    uint8_t envIndex;
    uint8_t index;

    explicit ByteCode(int ei, int i): code(BCE_LoadFree) {
        checkedAssign(&envIndex, ei);
        checkedAssign(&index, i);
    }
};

template<>
struct ByteCode<BCE_StoreFree> {
    uint8_t code;
    uint8_t envIndex;
    uint8_t index;

    explicit ByteCode(int ei, int i): code(BCE_StoreFree) {
        checkedAssign(&envIndex, ei);
        checkedAssign(&index, i);
    }
};

template<>
struct ByteCode<BCE_LoadFree1> {
    uint8_t code;
    uint8_t index;

    explicit ByteCode(int i): code(BCE_LoadFree1) {
        checkedAssign(&index, i);
    }
};

template<>
struct ByteCode<BCE_StoreFree1> {
    uint8_t code;
    uint8_t index;

    explicit ByteCode(int i): code(BCE_StoreFree1) {
        checkedAssign(&index, i);
    }
};

template<>
struct ByteCode<BCE_LoadFree2> {
    uint8_t code;
    uint8_t index;

    explicit ByteCode(int i): code(BCE_LoadFree2) {
        checkedAssign(&index, i);
    }
};

template<>
struct ByteCode<BCE_StoreFree2> {
    uint8_t code;
    uint8_t index;

    explicit ByteCode(int i): code(BCE_StoreFree2) {
        checkedAssign(&index, i);
    }
};

template<>
struct ByteCode<BCE_LoadFree3> {
    uint8_t code;
    uint8_t index;

    explicit ByteCode(int i): code(BCE_LoadFree3) {
        checkedAssign(&index, i);
    }
};

template<>
struct ByteCode<BCE_StoreFree3> {
    uint8_t code;
    uint8_t index;

    explicit ByteCode(int i): code(BCE_StoreFree3) {
        checkedAssign(&index, i);
    }
};

template<>
struct ByteCode<BCE_LoadFunc> {
    uint8_t code;
    uint16_t findex;

    explicit ByteCode(int i): code(BCE_LoadFunc) {
        checkedAssign(&findex, i);
    }
};

template<>
struct ByteCode<BCE_LoadClass> {
    uint8_t code;
    uint16_t cindex;

    explicit ByteCode(int i): code(BCE_LoadClass) {
        checkedAssign(&cindex, i);
    }
};

template<>
struct ByteCode<BCE_Pop> {
    uint8_t code;

    explicit ByteCode(): code(BCE_Pop) {
    }
};

template<>
struct ByteCode<BCE_Jmp> {
    uint8_t code;
    uint16_t target;

    explicit ByteCode(): code(BCE_Jmp), target(0) {
    }

    uint16_t* getTargetPtr() {
        return &target;
    }
};

template<>
struct ByteCode<BCE_TrueJmp> {
    uint8_t code;
    uint16_t target;

    explicit ByteCode(): code(BCE_TrueJmp), target(0) {
    }

    uint16_t* getTargetPtr() {
        return &target;
    }
};

template<>
struct ByteCode<BCE_Tail> {
    uint8_t code;

    explicit ByteCode(): code(BCE_Tail) {
    }
};

template<>
struct ByteCode<BCE_Call> {
    uint8_t code;
    uint8_t actualCount;

    explicit ByteCode(int i): code(BCE_Call) {
        checkedAssign(&actualCount, i);
    }
};

template<>
struct ByteCode<BCE_LoadCachedMethod> {
    uint8_t code;
    uint8_t cachedObjIndex;
    SFuncProto *cachedFunc;

    ByteCode() = delete;

    void updateCache(int oi, SFuncProto *f) {
        code = BCE_LoadCachedMethod;
        checkedAssign(&cachedObjIndex, oi);
        cachedFunc = f;
    }
};

template<>
struct ByteCode<BCE_LoadMethod> {
    uint8_t code;
    uint8_t envIndex;
    uint8_t index;
    uint8_t reserved[sizeof(SFuncProto*) - sizeof(uint8_t)];

    explicit ByteCode(int ei, int i): code(BCE_LoadMethod) {
        checkedAssign(&envIndex, ei);
        checkedAssign(&index, i);
    }
};

static_assert(sizeof(ByteCode<BCE_LoadMethod>) == sizeof(ByteCode<BCE_LoadCachedMethod>), "");

template<>
struct ByteCode<BCE_GetField> {
    uint8_t code;
    uint16_t kindex;
    char reserved[sizeof(SClassProto*) + sizeof(uint8_t)];

    explicit ByteCode(int i): code(BCE_GetField) {
        checkedAssign(&kindex, i);
    }
};

template<>
struct ByteCode<BCE_GetCachedField> {
    uint8_t code;
    uint16_t kindex;
    uint8_t cachedIndex;
    SClassProto *cachedClass;

    ByteCode() = delete;

    void updateCache(SClassProto *cproto, int i) {
        code = BCE_GetCachedField;
        cachedClass = cproto;
        checkedAssign(&cachedIndex, i);
    }

    void discardCache() {
        code = BCE_GetField;
    }
};
static_assert(sizeof(ByteCode<BCE_GetField>) == sizeof(ByteCode<BCE_GetCachedField>), "");

template<>
struct ByteCode<BCE_GetMethod> {
    uint8_t code;
    uint16_t kindex;
    char reserved[sizeof(SClassProto*) + sizeof(SFuncProto*)];

    explicit ByteCode(int i): code(BCE_GetMethod) {
        checkedAssign(&kindex, i);
    }
};

template<>
struct ByteCode<BCE_GetCachedMethod> {
    uint8_t code;
    uint16_t kindex;
    SFuncProto *cachedFunc;
    SClassProto *cachedClass;

    ByteCode() = delete;

    void updateCache(SClassProto *cproto, SFuncProto *fproto) {
        code = BCE_GetCachedMethod;
        cachedClass = cproto;
        cachedFunc = fproto;
    }

    void discardCache() {
        code = BCE_GetMethod;
    }
};
static_assert(sizeof(ByteCode<BCE_GetMethod>) == sizeof(ByteCode<BCE_GetCachedMethod>), "");

template<>
struct ByteCode<BCE_Inline_Add> { uint8_t code; ByteCode(): code(BCE_Inline_Add) {} };
template<>
struct ByteCode<BCE_Inline_Sub> { uint8_t code; ByteCode(): code(BCE_Inline_Sub) {} };
template<>
struct ByteCode<BCE_Inline_Mul> { uint8_t code; ByteCode(): code(BCE_Inline_Mul) {} };
template<>
struct ByteCode<BCE_Inline_Div> { uint8_t code; ByteCode(): code(BCE_Inline_Div) {} };
template<>
struct ByteCode<BCE_Inline_Quotient> { uint8_t code; ByteCode(): code(BCE_Inline_Quotient) {} };
template<>
struct ByteCode<BCE_Inline_Mod> { uint8_t code; ByteCode(): code(BCE_Inline_Mod) {} };
template<>
struct ByteCode<BCE_Inline_Num_Equal> { uint8_t code; ByteCode(): code(BCE_Inline_Num_Equal) {} };
template<>
struct ByteCode<BCE_Inline_Num_Less> { uint8_t code; ByteCode(): code(BCE_Inline_Num_Less) {} };
template<>
struct ByteCode<BCE_Inline_Num_LessEq> { uint8_t code; ByteCode(): code(BCE_Inline_Num_LessEq) {} };
template<>
struct ByteCode<BCE_Inline_Num_Greater> { uint8_t code; ByteCode(): code(BCE_Inline_Num_Greater) {} };
template<>
struct ByteCode<BCE_Inline_Num_GreaterEq> { uint8_t code; ByteCode(): code(BCE_Inline_Num_GreaterEq) {} };
template<>
struct ByteCode<BCE_Inline_Not> { uint8_t code; ByteCode(): code(BCE_Inline_Not) {} };
template<>
struct ByteCode<BCE_Inline_Eq> { uint8_t code; ByteCode(): code(BCE_Inline_Eq) {} };
template<>
struct ByteCode<BCE_Inline_Empty> { uint8_t code; ByteCode(): code(BCE_Inline_Empty) {} };
template<>
struct ByteCode<BCE_Inline_Cons> { uint8_t code; ByteCode(): code(BCE_Inline_Cons) {} };
template<>
struct ByteCode<BCE_Inline_Car> { uint8_t code; ByteCode(): code(BCE_Inline_Car) {} };
template<>
struct ByteCode<BCE_Inline_Cdr> { uint8_t code; ByteCode(): code(BCE_Inline_Cdr) {} };

#pragma pack(pop)

#endif
