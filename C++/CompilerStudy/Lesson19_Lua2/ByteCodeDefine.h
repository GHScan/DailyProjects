
#ifndef BYTE_CODE_DEFINE_H
#define BYTE_CODE_DEFINE_H

enum ByteCode {
    BC_Move,
    BC_LoadVArgs,
    BC_GetGlobal, BC_SetGlobal,

    BC_NewFunction, BC_NewTable,

    BC_Call, 
    BC_ExitBlock,

    BC_Less, BC_LessEq, BC_Greater, BC_GreaterEq, BC_Equal, BC_NEqual,
    BC_Add, BC_Sub, BC_Mul,
    BC_Div, BC_Mod, BC_Pow,
    BC_Concat,

    BC_Not, BC_Len, BC_Minus, // TODO: compress to one code

    BC_GetTable, BC_SetTable, 

    BC_Jump,
    BC_TrueJump, BC_FalseJump,

    BC_Nop, BC_ReturnN,
    BC_PushValues2Table,
};

class VarIndex {
public:
    VarIndex(int idx): m_idx(idx){}
    static VarIndex fromConst(int idx) { ASSERT(idx < 0x80); return VarIndex(0x80 | idx);}
    static VarIndex fromLocal(int idx) { ASSERT(idx < 0x40); return VarIndex(0x40 | idx);}
    static VarIndex fromUpValue(int idx) { ASSERT(idx < 0x40); return VarIndex(0x00 | idx);}
    bool isConst() const { return (m_idx >> 7) == 1;}
    bool isLocal() const { return (m_idx >> 6) == 1;}
    bool isUpValue() const { return (m_idx >> 6) == 0;}
    int getConstIdx() const { assert(isConst()); return m_idx & 0x7f;}
    int getLocalIdx() const { assert(isLocal()); return m_idx & 0x3f;}
    int getUpValueIdx() const { assert(isUpValue()); return m_idx & 0x3f;}
    int toInt() const { return m_idx; }
private:
    int m_idx;
};

#define BIT_W_VAR 8
#define BIT_W_IP 16
#define SET_CODE1(codeType, v1) { ASSERT(code == 0 && codeType < (1 << 8) && v1 < (1 << 24)); code = codeType | v1 << 8; }
#define SET_CODE2(codeType, bw1, bw2, v1, v2) {\
    ASSERT(code == 0 && codeType < (1 << 8) && v1 < (1 << bw1) && v2 < (1 << bw2));\
    static_assert(bw1 + bw2 <= 24, "");\
    code = codeType | v2 << 8 | v1 << (8 + v2);\
}
#define SET_CODE3(codeType, bw1, bw2, bw3, v1, v2, v3) {\
    ASSERT(code == 0 && codeType < (1 << 8) && v1 < (1 << bw1) && v2 < (1 << bw2) && v3 < (1 << bw3));\
    static_assert(bw1 + bw2 + bw3 <= 24, "");\
    code = codeType | v3 << 8 | v2 << (8 + bw3) | v1 << (8 + bw2 + bw3);\
}
#define GET_CODE1(v1) int v1; v1 = code >> 8;
#define GET_CODE2(bw1, bw2, v1, v2) \
    int v1, v2;\
    v2 = (code >> 8) & ((1 << bw2) - 1); \
    v1 = (code >> (8 + bw2)) & ((1 << bw1) - 1);
#define GET_CODE3(bw1, bw2, bw3, v1, v2, v3)  \
    int v1, v2, v3;\
    v3 = (code >> 8) & ((1 << bw3) - 1); \
    v2 = (code >> (8 + bw3)) & ((1 << bw2) - 1); \
    v1 = (code >> (8 + bw2 + bw3)) & ((1 << bw1) - 1);
#define GET_STRING_FROM_META(str, idx) \
    string str;\
    {\
        VarIndex varIdx(idx);\
        if (varIdx.isConst()) str = meta->constTable[varIdx.getConstIdx()].toString();\
        else if (varIdx.isLocal()) str = format("l_%d", varIdx.getLocalIdx());\
        else str = format("uv_%d", varIdx.getUpValueIdx());\
    }
#define GET_VAR_FROM_FRAME(var, idx) \
    LuaValue *var;\
    {\
        VarIndex varIdx(idx);\
        if (varIdx.isConst()) var = &static_cast<LuaFunction*>(frame->func)->meta->constTable[varIdx.getConstIdx()];\
        else if (varIdx.isLocal()) var = &frame->localPtr[varIdx.getLocalIdx()];\
        else var = &static_cast<LuaFunction*>(frame->func)->upValue(varIdx.getUpValueIdx());\
    }

template<int n>
struct ByteCodeHandler;

template<>
struct ByteCodeHandler<BC_Move> {
    static void emit(int &code, int destIdx, int srcIdx) {
        SET_CODE2(BC_Move, BIT_W_VAR, BIT_W_VAR, destIdx, srcIdx);
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        GET_CODE2(BIT_W_VAR, BIT_W_VAR, destIdx, srcIdx);
        GET_STRING_FROM_META(destStr, destIdx);
        GET_STRING_FROM_META(srcStr, srcIdx);
        so << format("move %s<-%s", destStr.c_str(), srcStr.c_str());
    }
    static void execute(int code, LuaStackFrame* frame) {
        GET_CODE2(BIT_W_VAR, BIT_W_VAR, destIdx, srcIdx);
        GET_VAR_FROM_FRAME(dest, destIdx);
        GET_VAR_FROM_FRAME(src, srcIdx);
        *dest = *src;
    }
};
template<>
struct ByteCodeHandler<BC_LoadVArgs> {
    static void emit(int &code, int destIdx, bool isMulti) {
        SET_CODE2(BC_LoadVArgs, BIT_W_VAR, 8, destIdx, isMulti ? 1 : 0);
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        GET_CODE2(BIT_W_VAR, 8, destIdx, isMulti);
        GET_STRING_FROM_META(destStr, destIdx);
        so << format("loadVArgs %s,%s", destStr.c_str(), isMulti == 1 ? "singleV" : "multiV");
    }
    static void execute(int code, LuaStackFrame* frame) {
        GET_CODE2(BIT_W_VAR, 8, destIdx, isMulti);
        GET_VAR_FROM_FRAME(dest, destIdx);
        if (isMulti == 1) {
            int n = frame->localPtr - frame->varParamPtr;
            if (n == 0) {
                *dest = LuaValue::NIL;
            } else {
                frame->setExtCount(n);
                frame->stack->reserveValueSpace(destIdx + n);
                for (auto p = frame->varParamPtr; p < frame->localPtr; ++p) {
                    *dest++ = *p;
                }
            }
        } else {
            *dest = frame->varParamPtr == frame->localPtr ? LuaValue::NIL : *frame->varParamPtr;
        }
    }
};
template<>
struct ByteCodeHandler<BC_GetGlobal> {
    static void emit(int &code, int destIdx, int kIdx) {
        SET_CODE2(BC_GetGlobal, BIT_W_VAR, BIT_W_VAR, destIdx, kIdx);
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        GET_CODE2(BIT_W_VAR, BIT_W_VAR, destIdx, kIdx);
        GET_STRING_FROM_META(destStr, destIdx);
        GET_STRING_FROM_META(kStr, kIdx);
        so << format("getglobal %s=_G[%s]", destStr.c_str(), kStr.c_str());
    }
    static void execute(int code, LuaStackFrame* frame) {
        GET_CODE2(BIT_W_VAR, BIT_W_VAR, destIdx, kIdx);
        GET_VAR_FROM_FRAME(dest, destIdx);
        GET_VAR_FROM_FRAME(k, kIdx);
        *dest = static_cast<LuaFunction*>(frame->func)->fenvTable->get(*k);
    }
};
template<>
struct ByteCodeHandler<BC_SetGlobal> {
    static void emit(int &code, int kIdx, int vIdx) {
        SET_CODE2(BC_SetGlobal, BIT_W_VAR, BIT_W_VAR, kIdx, vIdx);
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        GET_CODE2(BIT_W_VAR, BIT_W_VAR, kIdx, vIdx);
        GET_STRING_FROM_META(kStr, kIdx);
        GET_STRING_FROM_META(vStr, vIdx);
        so << format("setglobal _G[%s]=%s", kStr.c_str(), vStr.c_str());
    }
    static void execute(int code, LuaStackFrame* frame) {
        GET_CODE2(BIT_W_VAR, BIT_W_VAR, kIdx, vIdx);
        GET_VAR_FROM_FRAME(k, kIdx);
        GET_VAR_FROM_FRAME(v, vIdx);
        static_cast<LuaFunction*>(frame->func)->fenvTable->set(*k, *v);
    }
};
template<>
struct ByteCodeHandler<BC_NewFunction> {
    static void emit(int &code, int destIdx, int metaIdx) {
        SET_CODE2(BC_NewFunction, BIT_W_VAR, 16, destIdx, metaIdx);
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        GET_CODE2(BIT_W_VAR, 16, destIdx, metaIdx);
        GET_STRING_FROM_META(destStr, destIdx);
        so << format("newfunction %s=function(%d)", destStr.c_str(), metaIdx);
    }
    static void execute(int code, LuaStackFrame* frame) {
        GET_CODE2(BIT_W_VAR, 16, destIdx, metaIdx);
        GET_VAR_FROM_FRAME(dest, destIdx);
        *dest = LuaValue(LuaFunction::create(LuaVM::instance()->getMeta(metaIdx)));
    }
};
template<>
struct ByteCodeHandler<BC_NewTable> {
    static void emit(int &code, int destIdx) {
        SET_CODE1(BC_NewTable, destIdx);
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        GET_CODE1(destIdx);
        GET_STRING_FROM_META(destStr, destIdx);
        so << format("newtable %s={}", destStr.c_str());
    }
    static void execute(int code, LuaStackFrame* frame) {
        GET_CODE1(destIdx);
        GET_VAR_FROM_FRAME(dest, destIdx);
        *dest = LuaValue(LuaTable::create());
    }
};
template<>
struct ByteCodeHandler<BC_Call> {
    static void emit(int &code, int funcIdx, int paramCount, bool isMulti) {
        SET_CODE3(BC_Call, BIT_W_VAR, 8, 8, funcIdx, paramCount, isMulti ? 1 : 0);
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        GET_CODE3(BIT_W_VAR, 8, 8, funcIdx, paramCount, isMulti);
        GET_STRING_FROM_META(funcStr, funcIdx);
        so << format("call %s,%d,%s", funcStr.c_str(), paramCount, isMulti == 1 ? "singleV" : "multiV");
    }
    static void execute(int code, LuaStackFrame* frame) {
        GET_CODE3(BIT_W_VAR, 8, 8, funcIdx, paramCount, isMulti);
        callFunc(funcIdx, paramCount - 1 + frame->getExtCount(), isMulti == 1);
    }
};
template<>
struct ByteCodeHandler<BC_ExitBlock> {
    static void emit(int &code, int localOff, int localCount) {
        SET_CODE2(BC_ExitBlock, 8, 8, localOff, localCount);
    }
    static void emitOff(int &code, int localOff) {
        ASSERT(localOff < (1 << 8));
        code = (code & 0xffff) | (localOff << 16);
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        GET_CODE2(8, 8, localOff, localCount);
        so << format("exitBlock %d,%d", localOff, localCount);
    }
    static void execute(int code, LuaStackFrame* frame) {
        GET_CODE2(8, 8, localOff, localCount);
        if (localCount == 0) return;
        auto& closures = frame->closures;
        auto iter = closures.lower_bound(localOff);
        if (iter == closures.end()) return;
        auto _iter = iter;
        while (iter != closures.end()) {
            assert(iter->first >= localOff && iter->first < localOff + localCount);
            auto iter2 = iter;
            do {
                ++iter2;
            } while(iter2 != closures.end() && iter2->first == iter->first);
            shared_ptr<LuaValue> v(new LuaValue(frame->localPtr[iter->first]));
            for (; iter != iter2; ++iter) {
                auto& funcUvIdx = iter->second;
                funcUvIdx.first->sharedUpValues.push_back(v);
                funcUvIdx.first->upValues[funcUvIdx.second] = v.get();
            }
        }
        closures.erase(_iter, closures.end());
    }
};
template<>
struct ByteCodeHandler<BC_Less> {
    static void emit(int &code, int destIdx, int leftIdx, int rightIdx) {
        SET_CODE3(BC_Less, BIT_W_VAR, BIT_W_VAR, BIT_W_VAR, destIdx, leftIdx, rightIdx);
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        GET_CODE3(BIT_W_VAR, BIT_W_VAR, BIT_W_VAR, destIdx, leftIdx, rightIdx);
        GET_STRING_FROM_META(destStr, destIdx);
        GET_STRING_FROM_META(leftStr, leftIdx);
        GET_STRING_FROM_META(rightStr, rightIdx);
        so << format("less %s=%s < %s", destStr.c_str(), leftStr.c_str(), rightStr.c_str());
    }
    static void execute(int code, LuaStackFrame* frame) {
        GET_CODE3(BIT_W_VAR, BIT_W_VAR, BIT_W_VAR, destIdx, leftIdx, rightIdx);
        GET_VAR_FROM_FRAME(dest, destIdx);
        GET_VAR_FROM_FRAME(left, leftIdx);
        GET_VAR_FROM_FRAME(right, rightIdx);
        *dest = *left < *right ? LuaValue::TRUE : LuaValue::FALSE;
    }
};
template<>
struct ByteCodeHandler<BC_LessEq> {
    static void emit(int &code, int destIdx, int leftIdx, int rightIdx) {
        SET_CODE3(BC_LessEq, BIT_W_VAR, BIT_W_VAR, BIT_W_VAR, destIdx, leftIdx, rightIdx);
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        GET_CODE3(BIT_W_VAR, BIT_W_VAR, BIT_W_VAR, destIdx, leftIdx, rightIdx);
        GET_STRING_FROM_META(destStr, destIdx);
        GET_STRING_FROM_META(leftStr, leftIdx);
        GET_STRING_FROM_META(rightStr, rightIdx);
        so << format("lessEq %s=%s <= %s", destStr.c_str(), leftStr.c_str(), rightStr.c_str());
    }
    static void execute(int code, LuaStackFrame* frame) {
        GET_CODE3(BIT_W_VAR, BIT_W_VAR, BIT_W_VAR, destIdx, leftIdx, rightIdx);
        GET_VAR_FROM_FRAME(dest, destIdx);
        GET_VAR_FROM_FRAME(left, leftIdx);
        GET_VAR_FROM_FRAME(right, rightIdx);
        *dest = *left <= *right ? LuaValue::TRUE : LuaValue::FALSE;
    }
};
template<>
struct ByteCodeHandler<BC_Greater> {
    static void emit(int &code, int destIdx, int leftIdx, int rightIdx) {
        SET_CODE3(BC_Greater, BIT_W_VAR, BIT_W_VAR, BIT_W_VAR, destIdx, leftIdx, rightIdx);
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        GET_CODE3(BIT_W_VAR, BIT_W_VAR, BIT_W_VAR, destIdx, leftIdx, rightIdx);
        GET_STRING_FROM_META(destStr, destIdx);
        GET_STRING_FROM_META(leftStr, leftIdx);
        GET_STRING_FROM_META(rightStr, rightIdx);
        so << format("greate %s=%s > %s", destStr.c_str(), leftStr.c_str(), rightStr.c_str());
    }
    static void execute(int code, LuaStackFrame* frame) {
        GET_CODE3(BIT_W_VAR, BIT_W_VAR, BIT_W_VAR, destIdx, leftIdx, rightIdx);
        GET_VAR_FROM_FRAME(dest, destIdx);
        GET_VAR_FROM_FRAME(left, leftIdx);
        GET_VAR_FROM_FRAME(right, rightIdx);
        *dest = *left > *right ? LuaValue::TRUE : LuaValue::FALSE;
    }
};
template<>
struct ByteCodeHandler<BC_GreaterEq> {
    static void emit(int &code, int destIdx, int leftIdx, int rightIdx) {
        SET_CODE3(BC_GreaterEq, BIT_W_VAR, BIT_W_VAR, BIT_W_VAR, destIdx, leftIdx, rightIdx);
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        GET_CODE3(BIT_W_VAR, BIT_W_VAR, BIT_W_VAR, destIdx, leftIdx, rightIdx);
        GET_STRING_FROM_META(destStr, destIdx);
        GET_STRING_FROM_META(leftStr, leftIdx);
        GET_STRING_FROM_META(rightStr, rightIdx);
        so << format("greateEq %s=%s >= %s", destStr.c_str(), leftStr.c_str(), rightStr.c_str());
    }
    static void execute(int code, LuaStackFrame* frame) {
        GET_CODE3(BIT_W_VAR, BIT_W_VAR, BIT_W_VAR, destIdx, leftIdx, rightIdx);
        GET_VAR_FROM_FRAME(dest, destIdx);
        GET_VAR_FROM_FRAME(left, leftIdx);
        GET_VAR_FROM_FRAME(right, rightIdx);
        *dest = *left >= *right ? LuaValue::TRUE : LuaValue::FALSE;
    }
};
template<>
struct ByteCodeHandler<BC_Equal> {
    static void emit(int &code, int destIdx, int leftIdx, int rightIdx) {
        SET_CODE3(BC_Equal, BIT_W_VAR, BIT_W_VAR, BIT_W_VAR, destIdx, leftIdx, rightIdx);
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        GET_CODE3(BIT_W_VAR, BIT_W_VAR, BIT_W_VAR, destIdx, leftIdx, rightIdx);
        GET_STRING_FROM_META(destStr, destIdx);
        GET_STRING_FROM_META(leftStr, leftIdx);
        GET_STRING_FROM_META(rightStr, rightIdx);
        so << format("equal %s=%s == %s", destStr.c_str(), leftStr.c_str(), rightStr.c_str());
    }
    static void execute(int code, LuaStackFrame* frame) {
        GET_CODE3(BIT_W_VAR, BIT_W_VAR, BIT_W_VAR, destIdx, leftIdx, rightIdx);
        GET_VAR_FROM_FRAME(dest, destIdx);
        GET_VAR_FROM_FRAME(left, leftIdx);
        GET_VAR_FROM_FRAME(right, rightIdx);
        *dest = *left == *right ? LuaValue::TRUE : LuaValue::FALSE;
    }
};
template<>
struct ByteCodeHandler<BC_NEqual> {
    static void emit(int &code, int destIdx, int leftIdx, int rightIdx) {
        SET_CODE3(BC_NEqual, BIT_W_VAR, BIT_W_VAR, BIT_W_VAR, destIdx, leftIdx, rightIdx);
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        GET_CODE3(BIT_W_VAR, BIT_W_VAR, BIT_W_VAR, destIdx, leftIdx, rightIdx);
        GET_STRING_FROM_META(destStr, destIdx);
        GET_STRING_FROM_META(leftStr, leftIdx);
        GET_STRING_FROM_META(rightStr, rightIdx);
        so << format("nequal %s=%s ~= %s", destStr.c_str(), leftStr.c_str(), rightStr.c_str());
    }
    static void execute(int code, LuaStackFrame* frame) {
        GET_CODE3(BIT_W_VAR, BIT_W_VAR, BIT_W_VAR, destIdx, leftIdx, rightIdx);
        GET_VAR_FROM_FRAME(dest, destIdx);
        GET_VAR_FROM_FRAME(left, leftIdx);
        GET_VAR_FROM_FRAME(right, rightIdx);
        *dest = *left != *right ? LuaValue::TRUE : LuaValue::FALSE;
    }
};
template<>
struct ByteCodeHandler<BC_Add> {
    static void emit(int &code, int destIdx, int leftIdx, int rightIdx) {
        SET_CODE3(BC_Add, BIT_W_VAR, BIT_W_VAR, BIT_W_VAR, destIdx, leftIdx, rightIdx);
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        GET_CODE3(BIT_W_VAR, BIT_W_VAR, BIT_W_VAR, destIdx, leftIdx, rightIdx);
        GET_STRING_FROM_META(destStr, destIdx);
        GET_STRING_FROM_META(leftStr, leftIdx);
        GET_STRING_FROM_META(rightStr, rightIdx);
        so << format("add %s=%s + %s", destStr.c_str(), leftStr.c_str(), rightStr.c_str());
    }
    static void execute(int code, LuaStackFrame* frame) {
        GET_CODE3(BIT_W_VAR, BIT_W_VAR, BIT_W_VAR, destIdx, leftIdx, rightIdx);
        GET_VAR_FROM_FRAME(dest, destIdx);
        GET_VAR_FROM_FRAME(left, leftIdx);
        GET_VAR_FROM_FRAME(right, rightIdx);
        *dest = *left + *right;
    }
};
template<>
struct ByteCodeHandler<BC_Sub> {
    static void emit(int &code, int destIdx, int leftIdx, int rightIdx) {
        SET_CODE3(BC_Sub, BIT_W_VAR, BIT_W_VAR, BIT_W_VAR, destIdx, leftIdx, rightIdx);
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        GET_CODE3(BIT_W_VAR, BIT_W_VAR, BIT_W_VAR, destIdx, leftIdx, rightIdx);
        GET_STRING_FROM_META(destStr, destIdx);
        GET_STRING_FROM_META(leftStr, leftIdx);
        GET_STRING_FROM_META(rightStr, rightIdx);
        so << format("sub %s=%s - %s", destStr.c_str(), leftStr.c_str(), rightStr.c_str());
    }
    static void execute(int code, LuaStackFrame* frame) {
        GET_CODE3(BIT_W_VAR, BIT_W_VAR, BIT_W_VAR, destIdx, leftIdx, rightIdx);
        GET_VAR_FROM_FRAME(dest, destIdx);
        GET_VAR_FROM_FRAME(left, leftIdx);
        GET_VAR_FROM_FRAME(right, rightIdx);
        *dest = *left - *right;
    }
};
template<>
struct ByteCodeHandler<BC_Mul> {
    static void emit(int &code, int destIdx, int leftIdx, int rightIdx) {
        SET_CODE3(BC_Mul, BIT_W_VAR, BIT_W_VAR, BIT_W_VAR, destIdx, leftIdx, rightIdx);
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        GET_CODE3(BIT_W_VAR, BIT_W_VAR, BIT_W_VAR, destIdx, leftIdx, rightIdx);
        GET_STRING_FROM_META(destStr, destIdx);
        GET_STRING_FROM_META(leftStr, leftIdx);
        GET_STRING_FROM_META(rightStr, rightIdx);
        so << format("mul %s=%s * %s", destStr.c_str(), leftStr.c_str(), rightStr.c_str());
    }
    static void execute(int code, LuaStackFrame* frame) {
        GET_CODE3(BIT_W_VAR, BIT_W_VAR, BIT_W_VAR, destIdx, leftIdx, rightIdx);
        GET_VAR_FROM_FRAME(dest, destIdx);
        GET_VAR_FROM_FRAME(left, leftIdx);
        GET_VAR_FROM_FRAME(right, rightIdx);
        *dest = *left * *right;
    }
};
template<>
struct ByteCodeHandler<BC_Div> {
    static void emit(int &code, int destIdx, int leftIdx, int rightIdx) {
        SET_CODE3(BC_Div, BIT_W_VAR, BIT_W_VAR, BIT_W_VAR, destIdx, leftIdx, rightIdx);
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        GET_CODE3(BIT_W_VAR, BIT_W_VAR, BIT_W_VAR, destIdx, leftIdx, rightIdx);
        GET_STRING_FROM_META(destStr, destIdx);
        GET_STRING_FROM_META(leftStr, leftIdx);
        GET_STRING_FROM_META(rightStr, rightIdx);
        so << format("div %s=%s / %s", destStr.c_str(), leftStr.c_str(), rightStr.c_str());
    }
    static void execute(int code, LuaStackFrame* frame) {
        GET_CODE3(BIT_W_VAR, BIT_W_VAR, BIT_W_VAR, destIdx, leftIdx, rightIdx);
        GET_VAR_FROM_FRAME(dest, destIdx);
        GET_VAR_FROM_FRAME(left, leftIdx);
        GET_VAR_FROM_FRAME(right, rightIdx);
        *dest = *left / *right;
    }
};
template<>
struct ByteCodeHandler<BC_Mod> {
    static void emit(int &code, int destIdx, int leftIdx, int rightIdx) {
        SET_CODE3(BC_Mod, BIT_W_VAR, BIT_W_VAR, BIT_W_VAR, destIdx, leftIdx, rightIdx);
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        GET_CODE3(BIT_W_VAR, BIT_W_VAR, BIT_W_VAR, destIdx, leftIdx, rightIdx);
        GET_STRING_FROM_META(destStr, destIdx);
        GET_STRING_FROM_META(leftStr, leftIdx);
        GET_STRING_FROM_META(rightStr, rightIdx);
        so << format("mod %s=%s %% %s", destStr.c_str(), leftStr.c_str(), rightStr.c_str());
    }
    static void execute(int code, LuaStackFrame* frame) {
        GET_CODE3(BIT_W_VAR, BIT_W_VAR, BIT_W_VAR, destIdx, leftIdx, rightIdx);
        GET_VAR_FROM_FRAME(dest, destIdx);
        GET_VAR_FROM_FRAME(left, leftIdx);
        GET_VAR_FROM_FRAME(right, rightIdx);
        *dest = *left % *right;
    }
};
template<>
struct ByteCodeHandler<BC_Pow> {
    static void emit(int &code, int destIdx, int leftIdx, int rightIdx) {
        SET_CODE3(BC_Pow, BIT_W_VAR, BIT_W_VAR, BIT_W_VAR, destIdx, leftIdx, rightIdx);
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        GET_CODE3(BIT_W_VAR, BIT_W_VAR, BIT_W_VAR, destIdx, leftIdx, rightIdx);
        GET_STRING_FROM_META(destStr, destIdx);
        GET_STRING_FROM_META(leftStr, leftIdx);
        GET_STRING_FROM_META(rightStr, rightIdx);
        so << format("pow %s=%s ^ %s", destStr.c_str(), leftStr.c_str(), rightStr.c_str());
    }
    static void execute(int code, LuaStackFrame* frame) {
        GET_CODE3(BIT_W_VAR, BIT_W_VAR, BIT_W_VAR, destIdx, leftIdx, rightIdx);
        GET_VAR_FROM_FRAME(dest, destIdx);
        GET_VAR_FROM_FRAME(left, leftIdx);
        GET_VAR_FROM_FRAME(right, rightIdx);
        *dest = power(*left, *right);
    }
};
template<>
struct ByteCodeHandler<BC_Concat> {
    static void emit(int &code, int destIdx, int leftIdx, int rightIdx) {
        SET_CODE3(BC_Concat, BIT_W_VAR, BIT_W_VAR, BIT_W_VAR, destIdx, leftIdx, rightIdx);
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        GET_CODE3(BIT_W_VAR, BIT_W_VAR, BIT_W_VAR, destIdx, leftIdx, rightIdx);
        GET_STRING_FROM_META(destStr, destIdx);
        GET_STRING_FROM_META(leftStr, leftIdx);
        GET_STRING_FROM_META(rightStr, rightIdx);
        so << format("concat %s=%s .. %s", destStr.c_str(), leftStr.c_str(), rightStr.c_str());
    }
    static void execute(int code, LuaStackFrame* frame) {
        GET_CODE3(BIT_W_VAR, BIT_W_VAR, BIT_W_VAR, destIdx, leftIdx, rightIdx);
        GET_VAR_FROM_FRAME(dest, destIdx);
        GET_VAR_FROM_FRAME(left, leftIdx);
        GET_VAR_FROM_FRAME(right, rightIdx);
        *dest = concat(*left, *right);
    }
};
template<>
struct ByteCodeHandler<BC_Not> {
    static void emit(int &code, int destIdx, int srcIdx) {
        SET_CODE2(BC_Not, BIT_W_VAR, BIT_W_VAR, destIdx, srcIdx);
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        GET_CODE2(BIT_W_VAR, BIT_W_VAR, destIdx, srcIdx);
        GET_STRING_FROM_META(destStr, destIdx);
        GET_STRING_FROM_META(srcStr, srcIdx);
        so << format("not %s=not %s", destStr.c_str(), srcStr.c_str());
    }
    static void execute(int code, LuaStackFrame* frame) {
        GET_CODE2(BIT_W_VAR, BIT_W_VAR, destIdx, srcIdx);
        GET_VAR_FROM_FRAME(dest, destIdx);
        GET_VAR_FROM_FRAME(src, srcIdx);
        *dest = src->getBoolean() ? LuaValue::FALSE : LuaValue::TRUE;
    }
};
template<>
struct ByteCodeHandler<BC_Len> {
    static void emit(int &code, int destIdx, int srcIdx) {
        SET_CODE2(BC_Len, BIT_W_VAR, BIT_W_VAR, destIdx, srcIdx);
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        GET_CODE2(BIT_W_VAR, BIT_W_VAR, destIdx, srcIdx);
        GET_STRING_FROM_META(destStr, destIdx);
        GET_STRING_FROM_META(srcStr, srcIdx);
        so << format("len %s=#%s", destStr.c_str(), srcStr.c_str());
    }
    static void execute(int code, LuaStackFrame* frame) {
        GET_CODE2(BIT_W_VAR, BIT_W_VAR, destIdx, srcIdx);
        GET_VAR_FROM_FRAME(dest, destIdx);
        GET_VAR_FROM_FRAME(src, srcIdx);
        *dest = LuaValue(src->getSize());
    }
};
template<>
struct ByteCodeHandler<BC_Minus> {
    static void emit(int &code, int destIdx, int srcIdx) {
        SET_CODE2(BC_Minus, BIT_W_VAR, BIT_W_VAR, destIdx, srcIdx);
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        GET_CODE2(BIT_W_VAR, BIT_W_VAR, destIdx, srcIdx);
        GET_STRING_FROM_META(destStr, destIdx);
        GET_STRING_FROM_META(srcStr, srcIdx);
        so << format("minus %s=-%s", destStr.c_str(), srcStr.c_str());
    }
    static void execute(int code, LuaStackFrame* frame) {
        GET_CODE2(BIT_W_VAR, BIT_W_VAR, destIdx, srcIdx);
        GET_VAR_FROM_FRAME(dest, destIdx);
        GET_VAR_FROM_FRAME(src, srcIdx);
        if (src->isTypeOf(LVT_Table)) *dest = src->getTable()->meta_unm();
        else *dest = LuaValue(-src->getNumber());
    }
};
template<>
struct ByteCodeHandler<BC_GetTable> {
    static void emit(int &code, int destIdx, int tableIdx, int kIdx) {
        SET_CODE3(BC_GetTable, BIT_W_VAR, BIT_W_VAR, BIT_W_VAR, destIdx, tableIdx, kIdx);
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        GET_CODE3(BIT_W_VAR, BIT_W_VAR, BIT_W_VAR, destIdx, tableIdx, kIdx);
        GET_STRING_FROM_META(destStr, destIdx);
        GET_STRING_FROM_META(tableStr, tableIdx);
        GET_STRING_FROM_META(kStr, kIdx);
        so << format("gettable %s=%s[%s]", destStr.c_str(), tableStr.c_str(), kStr.c_str());
    }
    static void execute(int code, LuaStackFrame* frame) {
        GET_CODE3(BIT_W_VAR, BIT_W_VAR, BIT_W_VAR, destIdx, tableIdx, kIdx);
        GET_VAR_FROM_FRAME(dest, destIdx);
        GET_VAR_FROM_FRAME(table, tableIdx);
        GET_VAR_FROM_FRAME(k, kIdx);
        *dest = table->getTable()->get(*k);
    }
};
template<>
struct ByteCodeHandler<BC_SetTable> {
    static void emit(int &code, int tableIdx, int kIdx, int vIdx) {
        SET_CODE3(BC_SetTable, BIT_W_VAR, BIT_W_VAR, BIT_W_VAR, tableIdx, kIdx, vIdx);
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        GET_CODE3(BIT_W_VAR, BIT_W_VAR, BIT_W_VAR, tableIdx, kIdx, vIdx);
        GET_STRING_FROM_META(tableStr, tableIdx);
        GET_STRING_FROM_META(kStr, kIdx);
        GET_STRING_FROM_META(vStr, vIdx);
        so << format("settable %s[%s]=%s", tableStr.c_str(), kStr.c_str(), vStr.c_str());
    }
    static void execute(int code, LuaStackFrame* frame) {
        GET_CODE3(BIT_W_VAR, BIT_W_VAR, BIT_W_VAR, tableIdx, kIdx, vIdx);
        GET_VAR_FROM_FRAME(table, tableIdx);
        GET_VAR_FROM_FRAME(k, kIdx);
        GET_VAR_FROM_FRAME(v, vIdx);
        table->getTable()->set(*k, *v);
    }
};
template<>
struct ByteCodeHandler<BC_Jump> {
    static void emit(int &code, int ip) {
        SET_CODE1(BC_Jump, ip);
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        GET_CODE1(ip);
        so << format("jump %d", ip + 1);
    }
    static void execute(int code, LuaStackFrame* frame) {
        GET_CODE1(ip);
        frame->ip = ip - 1;
    }
};
template<>
struct ByteCodeHandler<BC_TrueJump> {
    static void emit(int &code, int idx, int ip) {
        SET_CODE2(BC_TrueJump, BIT_W_VAR, BIT_W_IP, idx, ip);
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        GET_CODE2(BIT_W_VAR, BIT_W_IP, idx, ip);
        GET_STRING_FROM_META(str, idx);
        so << format("tjump %s,%d", str.c_str(), ip + 1);
    }
    static void execute(int code, LuaStackFrame* frame) {
        GET_CODE2(BIT_W_VAR, BIT_W_IP, idx, ip);
        GET_VAR_FROM_FRAME(var, idx);
        if (var->getBoolean()) frame->ip = ip - 1;
    }
};
template<>
struct ByteCodeHandler<BC_FalseJump> {
    static void emit(int &code, int idx, int ip) {
        SET_CODE2(BC_FalseJump, BIT_W_VAR, BIT_W_IP, idx, ip);
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        GET_CODE2(BIT_W_VAR, BIT_W_IP, idx, ip);
        GET_STRING_FROM_META(str, idx);
        so << format("fjump %s,%d", str.c_str(), ip + 1);
    }
    static void execute(int code, LuaStackFrame* frame) {
        GET_CODE2(BIT_W_VAR, BIT_W_IP, idx, ip);
        GET_VAR_FROM_FRAME(var, idx);
        if (!var->getBoolean()) frame->ip = ip - 1;
    }
};
template<>
struct ByteCodeHandler<BC_Nop> {
    static void emit(int &code) {
        code = BC_Nop;
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        so << format("nop");
    }
    static void execute(int code, LuaStackFrame* frame) {
    }
};
template<>
struct ByteCodeHandler<BC_ReturnN> {
    static void emit(int &code, int paramCount) {
        SET_CODE1(BC_ReturnN, paramCount);
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        GET_CODE1(paramCount);
        so << format("returnN %d", paramCount);
    }
    static void execute(int code, LuaStackFrame* frame) {
        GET_CODE1(paramCount);
        frame->retN = paramCount - 1 + frame->getExtCount();
    }
};
template<>
struct ByteCodeHandler<BC_PushValues2Table> {
    static void emit(int &code, int tableIdx, int localIdx, int count) {
        SET_CODE3(BC_PushValues2Table, BIT_W_VAR, BIT_W_VAR, 8, tableIdx, localIdx, count);
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        GET_CODE3(BIT_W_VAR, BIT_W_VAR, 8, tableIdx, localIdx, count);
        GET_STRING_FROM_META(tableStr, tableIdx);
        GET_STRING_FROM_META(localStr, localIdx);
        so << format("pushValues2Table %s<-%s,%d", tableStr.c_str(), localStr.c_str(), count);
    }
    static void execute(int code, LuaStackFrame* frame) {
        GET_CODE3(BIT_W_VAR, BIT_W_VAR, 8, tableIdx, localIdx, count);
        GET_VAR_FROM_FRAME(table, tableIdx);
        GET_VAR_FROM_FRAME(local, localIdx);
        count += frame->getExtCount() - 1;
        for (int i = 0; i < count; ++i) {
            table->getTable()->arrayInsert(table->getSize(), *local++);
        }
    }
};

#undef BIT_W_VAR
#undef BIT_W_IP
#undef SET_CODE1
#undef SET_CODE2
#undef SET_CODE3
#undef GET_CODE1
#undef GET_CODE2
#undef GET_CODE3
#undef GET_STRING_FROM_META
#undef GET_VAR_FROM_FRAME

#endif
