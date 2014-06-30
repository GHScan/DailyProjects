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

    explicit ByteCode(): code(BCE_Jmp) {
    }

    uint16_t* getTargetPtr() {
        return &target;
    }
};

template<>
struct ByteCode<BCE_TrueJmp> {
    uint8_t code;
    uint16_t target;

    explicit ByteCode(): code(BCE_TrueJmp) {
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

#pragma pack(pop)

#endif
