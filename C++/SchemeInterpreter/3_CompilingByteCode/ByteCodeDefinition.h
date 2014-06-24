#ifndef BYTECODEDEFINITION_H
#define BYTECODEDEFINITION_H

enum ByteCodeEnums {
    BCE_LoadLiteral,
    BCE_LoadLocal,
    BCE_StoreLocal,
    BCE_LoadGlobal,
    BCE_StoreGlobal,
    BCE_LoadFree,
    BCE_StoreFree,
    BCE_LoadLambda,
    BCE_Jmp,
    BCE_TrueJmp,
    BCE_Tail,
    BCE_Call,
    BCE_Pop,
};

struct ByteCode_LoadLiteral {
    static const int CODE = BCE_LoadLiteral;
    uint8_t code;
    uint16_t literalIndex;

    explicit ByteCode_LoadLiteral(int _literalIndex): code(CODE) {
        checkedAssign(&literalIndex, _literalIndex);
    }
};

struct ByteCode_LoadLocal {
    static const int CODE = BCE_LoadLocal;
    uint8_t code;
    uint16_t localIndex;

    explicit ByteCode_LoadLocal(int _localIndex): code(CODE) {
        checkedAssign(&localIndex, _localIndex);
    }
};

struct ByteCode_StoreLocal {
    static const int CODE = BCE_StoreLocal;
    uint8_t code;
    uint16_t localIndex;

    explicit ByteCode_StoreLocal(int _localIndex): code(CODE) {
        checkedAssign(&localIndex, _localIndex);
    }
};

struct ByteCode_LoadFree {
    static const int CODE = BCE_LoadFree;
    uint8_t code;
    uint16_t freeIndex;

    explicit ByteCode_LoadFree(int _freeIndex): code(CODE) {
        checkedAssign(&freeIndex, _freeIndex);
    }
};

struct ByteCode_StoreFree {
    static const int CODE = BCE_StoreFree;
    uint8_t code;
    uint16_t freeIndex;

    explicit ByteCode_StoreFree(int _freeIndex): code(CODE) {
        checkedAssign(&freeIndex, _freeIndex);
    }
};

struct ByteCode_LoadGlobal {
    static const int CODE = BCE_LoadGlobal;
    uint8_t code;
    uint16_t globalIndex;

    explicit ByteCode_LoadGlobal(int _globalIndex): code(CODE) {
        checkedAssign(&globalIndex, _globalIndex);
    }
};

struct ByteCode_StoreGlobal {
    static const int CODE = BCE_StoreGlobal;
    uint8_t code;
    uint16_t globalIndex;

    explicit ByteCode_StoreGlobal(int _globalIndex): code(CODE) {
        checkedAssign(&globalIndex, _globalIndex);
    }
};

struct ByteCode_LoadLambda {
    static const int CODE = BCE_LoadLambda;
    uint8_t code;
    uint16_t protoIndex;

    explicit ByteCode_LoadLambda(int _protoIndex): code(CODE) {
        checkedAssign(&protoIndex, _protoIndex);
    }
};

struct ByteCode_Call {
    static const int CODE = BCE_Call;
    uint8_t code;
    uint16_t actualCount;

    explicit ByteCode_Call(int _actualCount): code(CODE) {
        checkedAssign(&actualCount, _actualCount);
    }
};

struct ByteCode_Tail{
    static const int CODE = BCE_Tail;
    uint8_t code;

    explicit ByteCode_Tail(): code(CODE) {
    }
};

struct ByteCode_Jmp {
    static const int CODE = BCE_Jmp;
    uint8_t code;
    uint16_t target;

    explicit ByteCode_Jmp(int _target): code(CODE) {
        checkedAssign(&target, _target);
    }
};

struct ByteCode_TrueJmp {
    static const int CODE = BCE_TrueJmp;
    uint8_t code;
    uint16_t target;

    explicit ByteCode_TrueJmp(int _target): code(CODE) {
        checkedAssign(&target, _target);
    }
};

struct ByteCode_Pop {
    static const int CODE = BCE_Pop;
    uint8_t code;

    explicit ByteCode_Pop(): code(CODE) {
    }
};

#endif
