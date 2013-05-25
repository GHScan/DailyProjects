
#ifndef BYTE_CODE_DEFINES_H
#define BYTE_CODE_DEFINES_H

#include "JSVM.h"
#include "JSFunction.h"
#include "JSArray.h"
#include "JSString.h"

enum ByteCodeType {
    BC_NewFunction, BC_NewArray,

    BC_Move,
    BC_Not, BC_Minus, BC_Len,
    BC_SetGlobal, BC_GetGlobal,

    BC_Add, BC_Sub, BC_Mul, BC_Div, BC_Mod, BC_Pow,
    BC_Less, BC_LessEq, BC_Greater, BC_GreaterEq, BC_Equal, BC_NEqual, 
    BC_SetArray, BC_GetArray,

    BC_Jump, BC_TrueJump, BC_FalseJump,

    BC_Call,
};

struct VarID {
    VarID(int id): m_id(id){}
    int getInt() const { return m_id; }
    static VarID fromLocal(int localIdx){ ASSERT(localIdx < 128); return VarID(localIdx);}
    static VarID fromConst(int constIdx){ ASSERT(constIdx < 128); return VarID(128 + constIdx);}
    bool isLocal() const { return (m_id >> 7) == 0; }
    bool isConst() const { return (m_id >> 7) == 1; }
    int getLocal() const { ASSERT(isLocal()); return m_id & 0x3f;}
    int getConst() const { ASSERT(isConst()); return m_id & 0x3f; }
    string toString(FuncMeta *meta) const {
        if (m_id >> 7) {
            auto v = meta->constTable[m_id & 0x3f];
            if (v.type == JSVT_String) return format("'%s'", v.data.str->buf);
            else return v.toString();
        }
        else return format("l_%d", m_id & 0x3f);
    }
    JSValue* toValue(JSValue* localConstPtr[2]) const{ return localConstPtr[m_id >> 7] + (m_id & 0x3f);}
private:
    const int m_id;
};


#define ENCODE_1(codeType, bw1, v1) \
    static_assert(bw1 + 8 <= 32, "");\
    ASSERT(codeType < (1 << 8));\
    ASSERT(v1 < (1 << bw1));\
    code = codeType | (v1 << 8);
#define ENCODE_2(codeType, bw1, bw2, v1, v2) \
    static_assert(bw1 + bw2 + 8 <= 32, "");\
    ASSERT(codeType < (1 << 8));\
    ASSERT(v1 < (1 << bw1)); ASSERT(v2 < (1 << bw2));\
    code = codeType | (v1 << 8) | (v2 << (8 + bw1));
#define ENCODE_3(codeType, bw1, bw2, bw3, v1, v2, v3) \
    static_assert(bw1 + bw2 + bw3 + 8 <= 32, "");\
    ASSERT(codeType < (1 << 8));\
    ASSERT(v1 < (1 << bw1)); ASSERT(v2 < (1 << bw2)); ASSERT(v3 < (1 << bw3));\
    code = codeType | (v1 << 8) | (v2 << (8 + bw1)) | (v3 << (8 + bw1 + bw2));
#define DECODE_1(bw1, v1) \
    int v1;\
    v1 = (code >> 8) & ((1 << bw1) - 1);
#define DECODE_2(bw1, bw2, v1, v2) \
    int v1, v2;\
    v1 = (code >> 8) & ((1 << bw1) - 1);\
    v2 = (code >> (8 + bw1)) & ((1 << bw2) - 1);
#define DECODE_3(bw1, bw2, bw3, v1, v2, v3) \
    int v1, v2, v3;\
    v1 = (code >> 8) & ((1 << bw1) - 1);\
    v2 = (code >> (8 + bw1)) & ((1 << bw2) - 1);\
    v3 = (code >> (8 + bw1 + bw2)) & ((1 << bw3) - 1);

static const int BIT_W_VAR_ID = 8;
static const int BIT_W_META_IDX = 8;
static const int BIT_W_IP = 16;

template<int>
struct ByteCodeHandler;

template<>
struct ByteCodeHandler<BC_NewFunction> {
    FORCE_INLINE static void emitCode(int &code, int destID, int metaIdx) {
        ENCODE_2(BC_NewFunction, BIT_W_VAR_ID, BIT_W_META_IDX, destID, metaIdx);
    }
    FORCE_INLINE static FuncMetaPtr getMetaFromCode(int code) {
        DECODE_2(BIT_W_VAR_ID, BIT_W_META_IDX, destID, metaIdx);
        (void)destID;
        return JSVM::instance()->getMetaFromIdx(metaIdx);
    }
    FORCE_INLINE static void execute(int code, StackFrame* frame) {
        DECODE_2(BIT_W_VAR_ID, BIT_W_META_IDX, destID, metaIdx);
        auto dest = VarID(destID).toValue(frame->localConstPtr);
        *dest = JSValue::fromFunction(JSFunction::create(JSVM::instance()->getMetaFromIdx(metaIdx)));
    }
    FORCE_INLINE static string disassemble(int code, FuncMeta* meta) {
        DECODE_2(BIT_W_VAR_ID, BIT_W_META_IDX, destID, metaIdx);
        return format("newfunction %s<-%d", VarID(destID).toString(meta).c_str(), metaIdx);
    }
};

template<>
struct ByteCodeHandler<BC_NewArray> {
    FORCE_INLINE static void emitCode(int &code, int destID, int srcID, int len) {
        ENCODE_3(BC_NewArray, BIT_W_VAR_ID, BIT_W_VAR_ID, 8, destID, srcID, len);
    }
    FORCE_INLINE static void execute(int code, StackFrame* frame) {
        DECODE_3(BIT_W_VAR_ID, BIT_W_VAR_ID, 8, destID, srcID, len);
        auto dest = VarID(destID).toValue(frame->localConstPtr);
        auto array = new JSArray();
        if (len > 0) {
            auto src = VarID(srcID).toValue(frame->localConstPtr);
            array->array.insert(array->array.end(), src, src + len);
        }
        *dest = JSValue::fromArray(array);
    }
    FORCE_INLINE static string disassemble(int code, FuncMeta* meta) {
        DECODE_3(BIT_W_VAR_ID, BIT_W_VAR_ID, 8, destID, srcID, len);
        return format("newarray %s<-%s,%d", VarID(destID).toString(meta).c_str(), VarID(srcID).toString(meta).c_str(), len);
    }
};
template<>
struct ByteCodeHandler<BC_Move> {
    FORCE_INLINE static void emitCode(int &code, int destID, int srcID) {
        ENCODE_2(BC_Move, BIT_W_VAR_ID, BIT_W_VAR_ID, destID, srcID);
    }
    FORCE_INLINE static void execute(int code, StackFrame* frame) {
        DECODE_2(BIT_W_VAR_ID, BIT_W_VAR_ID, destID, srcID);
        auto dest = VarID(destID).toValue(frame->localConstPtr);
        auto src = VarID(srcID).toValue(frame->localConstPtr);
        *dest = *src;
    }
    FORCE_INLINE static string disassemble(int code, FuncMeta* meta) {
        DECODE_2(BIT_W_VAR_ID, BIT_W_VAR_ID, destID, srcID);
        return format("move %s<-%s", VarID(destID).toString(meta).c_str(), VarID(srcID).toString(meta).c_str());
    }
};
template<>
struct ByteCodeHandler<BC_Not> {
    FORCE_INLINE static void emitCode(int &code, int destID, int srcID) {
        ENCODE_2(BC_Not, BIT_W_VAR_ID, BIT_W_VAR_ID, destID, srcID);
    }
    FORCE_INLINE static void execute(int code, StackFrame* frame) {
        DECODE_2(BIT_W_VAR_ID, BIT_W_VAR_ID, destID, srcID);
        auto dest = VarID(destID).toValue(frame->localConstPtr);
        auto src = VarID(srcID).toValue(frame->localConstPtr);
        dest->data.b = !src->getBoolean();
        dest->type = JSVT_Boolean;
    }
    FORCE_INLINE static string disassemble(int code, FuncMeta* meta) {
        DECODE_2(BIT_W_VAR_ID, BIT_W_VAR_ID, destID, srcID);
        return format("not %s<-%s", VarID(destID).toString(meta).c_str(), VarID(srcID).toString(meta).c_str());
    }
};
template<>
struct ByteCodeHandler<BC_Len> {
    FORCE_INLINE static void emitCode(int &code, int destID, int srcID) {
        ENCODE_2(BC_Len, BIT_W_VAR_ID, BIT_W_VAR_ID, destID, srcID);
    }
    FORCE_INLINE static void execute(int code, StackFrame* frame) {
        DECODE_2(BIT_W_VAR_ID, BIT_W_VAR_ID, destID, srcID);
        auto dest = VarID(destID).toValue(frame->localConstPtr);
        auto src = VarID(srcID).toValue(frame->localConstPtr);
        ASSERT(src->type == JSVT_Array);
        dest->data.num = (int)src->data.array->array.size();
        dest->type = JSVT_Number;
    }
    FORCE_INLINE static string disassemble(int code, FuncMeta* meta) {
        DECODE_2(BIT_W_VAR_ID, BIT_W_VAR_ID, destID, srcID);
        return format("len %s<-#%s", VarID(destID).toString(meta).c_str(), VarID(srcID).toString(meta).c_str());
    }
};
template<>
struct ByteCodeHandler<BC_Minus> {
    FORCE_INLINE static void emitCode(int &code, int destID, int srcID) {
        ENCODE_2(BC_Minus, BIT_W_VAR_ID, BIT_W_VAR_ID, destID, srcID);
    }
    FORCE_INLINE static void execute(int code, StackFrame* frame) {
        DECODE_2(BIT_W_VAR_ID, BIT_W_VAR_ID, destID, srcID);
        auto dest = VarID(destID).toValue(frame->localConstPtr);
        auto src = VarID(srcID).toValue(frame->localConstPtr);
        ASSERT(src->type == JSVT_Number);
        dest->data.num = -src->data.num;
        dest->type = JSVT_Number;
    }
    FORCE_INLINE static string disassemble(int code, FuncMeta* meta) {
        DECODE_2(BIT_W_VAR_ID, BIT_W_VAR_ID, destID, srcID);
        return format("minus %s<-%s", VarID(destID).toString(meta).c_str(), VarID(srcID).toString(meta).c_str());
    }
};
template<>
struct ByteCodeHandler<BC_SetGlobal> {
    FORCE_INLINE static void emitCode(int &code, int kID, int vID) {
        ENCODE_2(BC_SetGlobal, BIT_W_VAR_ID, BIT_W_VAR_ID, kID, vID);
    }
    FORCE_INLINE static void execute(int code, StackFrame* frame) {
        DECODE_2(BIT_W_VAR_ID, BIT_W_VAR_ID, kID, vID);
        auto k = VarID(kID).toValue(frame->localConstPtr);
        auto v = VarID(vID).toValue(frame->localConstPtr);
        JSVM::instance()->setGlobal(*k, *v);
    }
    FORCE_INLINE static string disassemble(int code, FuncMeta* meta) {
        DECODE_2(BIT_W_VAR_ID, BIT_W_VAR_ID, kID, vID);
        return format("setglobal _G[%s]=%s", VarID(kID).toString(meta).c_str(), VarID(vID).toString(meta).c_str());
    }
};
template<>
struct ByteCodeHandler<BC_GetGlobal> {
    FORCE_INLINE static void emitCode(int &code, int destID, int kID) {
        ENCODE_2(BC_GetGlobal, BIT_W_VAR_ID, BIT_W_VAR_ID, destID, kID);
    }
    FORCE_INLINE static void execute(int code, StackFrame* frame) {
        DECODE_2(BIT_W_VAR_ID, BIT_W_VAR_ID, destID, kID);
        auto dest = VarID(destID).toValue(frame->localConstPtr);
        auto k = VarID(kID).toValue(frame->localConstPtr);
        *dest = JSVM::instance()->getGlobal(*k);
    }
    FORCE_INLINE static string disassemble(int code, FuncMeta* meta) {
        DECODE_2(BIT_W_VAR_ID, BIT_W_VAR_ID, destID, kID);
        return format("getglobal %s=_G[%s]", VarID(destID).toString(meta).c_str(), VarID(kID).toString(meta).c_str());
    }
};
template<>
struct ByteCodeHandler<BC_Add> {
    FORCE_INLINE static void emitCode(int &code, int destID, int lID, int rID) {
        ENCODE_3(BC_Add, BIT_W_VAR_ID, BIT_W_VAR_ID, BIT_W_VAR_ID, destID, lID, rID);
    }
    FORCE_INLINE static void execute(int code, StackFrame* frame) {
        DECODE_3(BIT_W_VAR_ID, BIT_W_VAR_ID, BIT_W_VAR_ID, destID, lID, rID);
        auto dest = VarID(destID).toValue(frame->localConstPtr);
        auto l = VarID(lID).toValue(frame->localConstPtr);
        auto r = VarID(rID).toValue(frame->localConstPtr);
        if (l->type == JSVT_Number) {
            ASSERT(r->type == JSVT_Number);
            dest->data.num = l->data.num + r->data.num;
            dest->type = JSVT_Number;
        } else if (l->type == JSVT_String) {
            *dest = JSValue::fromString((string(l->data.str->buf) + r->toString()).c_str());
        } else {
            ASSERT(0);
        }
    }
    FORCE_INLINE static string disassemble(int code, FuncMeta* meta) {
        DECODE_3(BIT_W_VAR_ID, BIT_W_VAR_ID, BIT_W_VAR_ID, destID, lID, rID);
        return format("add %s=%s+%s", VarID(destID).toString(meta).c_str(), VarID(lID).toString(meta).c_str(), VarID(rID).toString(meta).c_str());
    }
};
template<>
struct ByteCodeHandler<BC_Sub> {
    FORCE_INLINE static void emitCode(int &code, int destID, int lID, int rID) {
        ENCODE_3(BC_Sub, BIT_W_VAR_ID, BIT_W_VAR_ID, BIT_W_VAR_ID, destID, lID, rID);
    }
    FORCE_INLINE static void execute(int code, StackFrame* frame) {
        DECODE_3(BIT_W_VAR_ID, BIT_W_VAR_ID, BIT_W_VAR_ID, destID, lID, rID);
        auto dest = VarID(destID).toValue(frame->localConstPtr);
        auto l = VarID(lID).toValue(frame->localConstPtr);
        auto r = VarID(rID).toValue(frame->localConstPtr);
        ASSERT(l->type == JSVT_Number && r->type == JSVT_Number);
        dest->data.num = l->data.num - r->data.num;
        dest->type = JSVT_Number;
    }
    FORCE_INLINE static string disassemble(int code, FuncMeta* meta) {
        DECODE_3(BIT_W_VAR_ID, BIT_W_VAR_ID, BIT_W_VAR_ID, destID, lID, rID);
        return format("sub %s=%s-%s", VarID(destID).toString(meta).c_str(), VarID(lID).toString(meta).c_str(), VarID(rID).toString(meta).c_str());
    }
};
template<>
struct ByteCodeHandler<BC_Mul> {
    FORCE_INLINE static void emitCode(int &code, int destID, int lID, int rID) {
        ENCODE_3(BC_Mul, BIT_W_VAR_ID, BIT_W_VAR_ID, BIT_W_VAR_ID, destID, lID, rID);
    }
    FORCE_INLINE static void execute(int code, StackFrame* frame) {
        DECODE_3(BIT_W_VAR_ID, BIT_W_VAR_ID, BIT_W_VAR_ID, destID, lID, rID);
        auto dest = VarID(destID).toValue(frame->localConstPtr);
        auto l = VarID(lID).toValue(frame->localConstPtr);
        auto r = VarID(rID).toValue(frame->localConstPtr);
        ASSERT(l->type == JSVT_Number && r->type == JSVT_Number);
        dest->data.num = l->data.num * r->data.num;
        dest->type = JSVT_Number;
    }
    FORCE_INLINE static string disassemble(int code, FuncMeta* meta) {
        DECODE_3(BIT_W_VAR_ID, BIT_W_VAR_ID, BIT_W_VAR_ID, destID, lID, rID);
        return format("mul %s=%s*%s", VarID(destID).toString(meta).c_str(), VarID(lID).toString(meta).c_str(), VarID(rID).toString(meta).c_str());
    }
};
template<>
struct ByteCodeHandler<BC_Div> {
    FORCE_INLINE static void emitCode(int &code, int destID, int lID, int rID) {
        ENCODE_3(BC_Div, BIT_W_VAR_ID, BIT_W_VAR_ID, BIT_W_VAR_ID, destID, lID, rID);
    }
    FORCE_INLINE static void execute(int code, StackFrame* frame) {
        DECODE_3(BIT_W_VAR_ID, BIT_W_VAR_ID, BIT_W_VAR_ID, destID, lID, rID);
        auto dest = VarID(destID).toValue(frame->localConstPtr);
        auto l = VarID(lID).toValue(frame->localConstPtr);
        auto r = VarID(rID).toValue(frame->localConstPtr);
        ASSERT(l->type == JSVT_Number && r->type == JSVT_Number);
        dest->data.num = l->data.num / r->data.num;
        dest->type = JSVT_Number;
    }
    FORCE_INLINE static string disassemble(int code, FuncMeta* meta) {
        DECODE_3(BIT_W_VAR_ID, BIT_W_VAR_ID, BIT_W_VAR_ID, destID, lID, rID);
        return format("div %s=%s/%s", VarID(destID).toString(meta).c_str(), VarID(lID).toString(meta).c_str(), VarID(rID).toString(meta).c_str());
    }
};
template<>
struct ByteCodeHandler<BC_Mod> {
    FORCE_INLINE static void emitCode(int &code, int destID, int lID, int rID) {
        ENCODE_3(BC_Mod, BIT_W_VAR_ID, BIT_W_VAR_ID, BIT_W_VAR_ID, destID, lID, rID);
    }
    FORCE_INLINE static void execute(int code, StackFrame* frame) {
        DECODE_3(BIT_W_VAR_ID, BIT_W_VAR_ID, BIT_W_VAR_ID, destID, lID, rID);
        auto dest = VarID(destID).toValue(frame->localConstPtr);
        auto l = VarID(lID).toValue(frame->localConstPtr);
        auto r = VarID(rID).toValue(frame->localConstPtr);
        ASSERT(l->type == JSVT_Number && r->type == JSVT_Number);
        dest->data.num = ::fmod(l->data.num, r->data.num);
        dest->type = JSVT_Number;
    }
    FORCE_INLINE static string disassemble(int code, FuncMeta* meta) {
        DECODE_3(BIT_W_VAR_ID, BIT_W_VAR_ID, BIT_W_VAR_ID, destID, lID, rID);
        return format("mod %s=%s%%%s", VarID(destID).toString(meta).c_str(), VarID(lID).toString(meta).c_str(), VarID(rID).toString(meta).c_str());
    }
};
template<>
struct ByteCodeHandler<BC_Pow> {
    FORCE_INLINE static void emitCode(int &code, int destID, int lID, int rID) {
        ENCODE_3(BC_Pow, BIT_W_VAR_ID, BIT_W_VAR_ID, BIT_W_VAR_ID, destID, lID, rID);
    }
    FORCE_INLINE static void execute(int code, StackFrame* frame) {
        DECODE_3(BIT_W_VAR_ID, BIT_W_VAR_ID, BIT_W_VAR_ID, destID, lID, rID);
        auto dest = VarID(destID).toValue(frame->localConstPtr);
        auto l = VarID(lID).toValue(frame->localConstPtr);
        auto r = VarID(rID).toValue(frame->localConstPtr);
        ASSERT(l->type == JSVT_Number && r->type == JSVT_Number);
        dest->data.num = ::pow(l->data.num, r->data.num);
        dest->type = JSVT_Number;
    }
    FORCE_INLINE static string disassemble(int code, FuncMeta* meta) {
        DECODE_3(BIT_W_VAR_ID, BIT_W_VAR_ID, BIT_W_VAR_ID, destID, lID, rID);
        return format("pow %s=%s^%s", VarID(destID).toString(meta).c_str(), VarID(lID).toString(meta).c_str(), VarID(rID).toString(meta).c_str());
    }
};
template<>
struct ByteCodeHandler<BC_Less> {
    FORCE_INLINE static void emitCode(int &code, int destID, int lID, int rID) {
        ENCODE_3(BC_Less, BIT_W_VAR_ID, BIT_W_VAR_ID, BIT_W_VAR_ID, destID, lID, rID);
    }
    FORCE_INLINE static void execute(int code, StackFrame* frame) {
        DECODE_3(BIT_W_VAR_ID, BIT_W_VAR_ID, BIT_W_VAR_ID, destID, lID, rID);
        auto dest = VarID(destID).toValue(frame->localConstPtr);
        auto l = VarID(lID).toValue(frame->localConstPtr);
        auto r = VarID(rID).toValue(frame->localConstPtr);
        ASSERT(l->type == JSVT_Number && r->type == JSVT_Number);
        dest->data.b = l->data.num < r->data.num;
        dest->type = JSVT_Boolean;
    }
    FORCE_INLINE static string disassemble(int code, FuncMeta* meta) {
        DECODE_3(BIT_W_VAR_ID, BIT_W_VAR_ID, BIT_W_VAR_ID, destID, lID, rID);
        return format("less %s=%s<%s", VarID(destID).toString(meta).c_str(), VarID(lID).toString(meta).c_str(), VarID(rID).toString(meta).c_str());
    }
};
template<>
struct ByteCodeHandler<BC_LessEq> {
    FORCE_INLINE static void emitCode(int &code, int destID, int lID, int rID) {
        ENCODE_3(BC_LessEq, BIT_W_VAR_ID, BIT_W_VAR_ID, BIT_W_VAR_ID, destID, lID, rID);
    }
    FORCE_INLINE static void execute(int code, StackFrame* frame) {
        DECODE_3(BIT_W_VAR_ID, BIT_W_VAR_ID, BIT_W_VAR_ID, destID, lID, rID);
        auto dest = VarID(destID).toValue(frame->localConstPtr);
        auto l = VarID(lID).toValue(frame->localConstPtr);
        auto r = VarID(rID).toValue(frame->localConstPtr);
        ASSERT(l->type == JSVT_Number && r->type == JSVT_Number);
        dest->data.b = l->data.num <= r->data.num;
        dest->type = JSVT_Boolean;
    }
    FORCE_INLINE static string disassemble(int code, FuncMeta* meta) {
        DECODE_3(BIT_W_VAR_ID, BIT_W_VAR_ID, BIT_W_VAR_ID, destID, lID, rID);
        return format("lessEq %s=%s<=%s", VarID(destID).toString(meta).c_str(), VarID(lID).toString(meta).c_str(), VarID(rID).toString(meta).c_str());
    }
};
template<>
struct ByteCodeHandler<BC_Greater> {
    FORCE_INLINE static void emitCode(int &code, int destID, int lID, int rID) {
        ENCODE_3(BC_Greater, BIT_W_VAR_ID, BIT_W_VAR_ID, BIT_W_VAR_ID, destID, lID, rID);
    }
    FORCE_INLINE static void execute(int code, StackFrame* frame) {
        DECODE_3(BIT_W_VAR_ID, BIT_W_VAR_ID, BIT_W_VAR_ID, destID, lID, rID);
        auto dest = VarID(destID).toValue(frame->localConstPtr);
        auto l = VarID(lID).toValue(frame->localConstPtr);
        auto r = VarID(rID).toValue(frame->localConstPtr);
        ASSERT(l->type == JSVT_Number && r->type == JSVT_Number);
        dest->data.b = l->data.num > r->data.num;
        dest->type = JSVT_Boolean;
    }
    FORCE_INLINE static string disassemble(int code, FuncMeta* meta) {
        DECODE_3(BIT_W_VAR_ID, BIT_W_VAR_ID, BIT_W_VAR_ID, destID, lID, rID);
        return format("greater %s=%s>%s", VarID(destID).toString(meta).c_str(), VarID(lID).toString(meta).c_str(), VarID(rID).toString(meta).c_str());
    }
};
template<>
struct ByteCodeHandler<BC_GreaterEq> {
    FORCE_INLINE static void emitCode(int &code, int destID, int lID, int rID) {
        ENCODE_3(BC_GreaterEq, BIT_W_VAR_ID, BIT_W_VAR_ID, BIT_W_VAR_ID, destID, lID, rID);
    }
    FORCE_INLINE static void execute(int code, StackFrame* frame) {
        DECODE_3(BIT_W_VAR_ID, BIT_W_VAR_ID, BIT_W_VAR_ID, destID, lID, rID);
        auto dest = VarID(destID).toValue(frame->localConstPtr);
        auto l = VarID(lID).toValue(frame->localConstPtr);
        auto r = VarID(rID).toValue(frame->localConstPtr);
        ASSERT(l->type == JSVT_Number && r->type == JSVT_Number);
        dest->data.b = l->data.num >= r->data.num;
        dest->type = JSVT_Boolean;
    }
    FORCE_INLINE static string disassemble(int code, FuncMeta* meta) {
        DECODE_3(BIT_W_VAR_ID, BIT_W_VAR_ID, BIT_W_VAR_ID, destID, lID, rID);
        return format("greaterEq %s=%s>=%s", VarID(destID).toString(meta).c_str(), VarID(lID).toString(meta).c_str(), VarID(rID).toString(meta).c_str());
    }
};
template<>
struct ByteCodeHandler<BC_Equal> {
    FORCE_INLINE static void emitCode(int &code, int destID, int lID, int rID) {
        ENCODE_3(BC_Equal, BIT_W_VAR_ID, BIT_W_VAR_ID, BIT_W_VAR_ID, destID, lID, rID);
    }
    FORCE_INLINE static void execute(int code, StackFrame* frame) {
        DECODE_3(BIT_W_VAR_ID, BIT_W_VAR_ID, BIT_W_VAR_ID, destID, lID, rID);
        auto dest = VarID(destID).toValue(frame->localConstPtr);
        auto l = VarID(lID).toValue(frame->localConstPtr);
        auto r = VarID(rID).toValue(frame->localConstPtr);
        ASSERT(l->type == JSVT_Number && r->type == JSVT_Number);
        dest->data.b = l->data.num == r->data.num;
        dest->type = JSVT_Boolean;
    }
    FORCE_INLINE static string disassemble(int code, FuncMeta* meta) {
        DECODE_3(BIT_W_VAR_ID, BIT_W_VAR_ID, BIT_W_VAR_ID, destID, lID, rID);
        return format("equal %s=%s==%s", VarID(destID).toString(meta).c_str(), VarID(lID).toString(meta).c_str(), VarID(rID).toString(meta).c_str());
    }
};
template<>
struct ByteCodeHandler<BC_NEqual> {
    FORCE_INLINE static void emitCode(int &code, int destID, int lID, int rID) {
        ENCODE_3(BC_NEqual, BIT_W_VAR_ID, BIT_W_VAR_ID, BIT_W_VAR_ID, destID, lID, rID);
    }
    FORCE_INLINE static void execute(int code, StackFrame* frame) {
        DECODE_3(BIT_W_VAR_ID, BIT_W_VAR_ID, BIT_W_VAR_ID, destID, lID, rID);
        auto dest = VarID(destID).toValue(frame->localConstPtr);
        auto l = VarID(lID).toValue(frame->localConstPtr);
        auto r = VarID(rID).toValue(frame->localConstPtr);
        ASSERT(l->type == JSVT_Number && r->type == JSVT_Number);
        dest->data.b = l->data.num != r->data.num;
        dest->type = JSVT_Boolean;
    }
    FORCE_INLINE static string disassemble(int code, FuncMeta* meta) {
        DECODE_3(BIT_W_VAR_ID, BIT_W_VAR_ID, BIT_W_VAR_ID, destID, lID, rID);
        return format("nequal %s=%s!=%s", VarID(destID).toString(meta).c_str(), VarID(lID).toString(meta).c_str(), VarID(rID).toString(meta).c_str());
    }
};
template<>
struct ByteCodeHandler<BC_SetArray> {
    FORCE_INLINE static void emitCode(int &code, int arrayID, int kID, int vID) {
        ENCODE_3(BC_SetArray, BIT_W_VAR_ID, BIT_W_VAR_ID, BIT_W_VAR_ID, arrayID, kID, vID);
    }
    FORCE_INLINE static void execute(int code, StackFrame* frame) {
        DECODE_3(BIT_W_VAR_ID, BIT_W_VAR_ID, BIT_W_VAR_ID, arrayID, kID, vID);
        auto array = VarID(arrayID).toValue(frame->localConstPtr);
        auto k = VarID(kID).toValue(frame->localConstPtr);
        auto v = VarID(vID).toValue(frame->localConstPtr);
        ASSERT(array->type == JSVT_Array && k->type == JSVT_Number);
        int i = (int)k->data.num;
        auto &vec = array->data.array->array;
        ASSERT(i >= 0 && i < (int)vec.size());
        vec[i] = *v;
    }
    FORCE_INLINE static string disassemble(int code, FuncMeta* meta) {
        DECODE_3(BIT_W_VAR_ID, BIT_W_VAR_ID, BIT_W_VAR_ID, arrayID, kID, vID);
        return format("setarray %s[%s]=%s", VarID(arrayID).toString(meta).c_str(), VarID(kID).toString(meta).c_str(), VarID(vID).toString(meta).c_str());
    }
};
template<>
struct ByteCodeHandler<BC_GetArray> {
    FORCE_INLINE static void emitCode(int &code, int destID, int arrayID, int kID) {
        ENCODE_3(BC_GetArray, BIT_W_VAR_ID, BIT_W_VAR_ID, BIT_W_VAR_ID, destID, arrayID, kID);
    }
    FORCE_INLINE static void execute(int code, StackFrame* frame) {
        DECODE_3(BIT_W_VAR_ID, BIT_W_VAR_ID, BIT_W_VAR_ID, destID, arrayID, kID);
        auto dest = VarID(destID).toValue(frame->localConstPtr);
        auto array = VarID(arrayID).toValue(frame->localConstPtr);
        auto k = VarID(kID).toValue(frame->localConstPtr);
        ASSERT(array->type == JSVT_Array && k->type == JSVT_Number);
        int i = (int)k->data.num;
        auto &vec = array->data.array->array;
        ASSERT(i >= 0 && i < (int)vec.size());
        *dest = vec[i];
    }
    FORCE_INLINE static string disassemble(int code, FuncMeta* meta) {
        DECODE_3(BIT_W_VAR_ID, BIT_W_VAR_ID, BIT_W_VAR_ID, destID, arrayID, kID);
        return format("getarray %s=%s[%s]", VarID(destID).toString(meta).c_str(), VarID(arrayID).toString(meta).c_str(), VarID(kID).toString(meta).c_str());
    }
};
template<>
struct ByteCodeHandler<BC_Jump> {
    FORCE_INLINE static void emitCode(int &code, int ip) {
        ENCODE_1(BC_Jump, BIT_W_IP, ip);
    }
    FORCE_INLINE static void execute(int code, StackFrame* frame) {
        DECODE_1(BIT_W_IP, ip);
        frame->ip = ip - 1;
    }
    FORCE_INLINE static string disassemble(int code, FuncMeta* meta) {
        DECODE_1(BIT_W_IP, ip);
        return format("jump %d", ip + 1);
    }
};
template<>
struct ByteCodeHandler<BC_TrueJump> {
    FORCE_INLINE static void emitCode(int &code, int testID, int ip) {
        ENCODE_2(BC_TrueJump, BIT_W_VAR_ID, BIT_W_IP, testID, ip);
    }
    FORCE_INLINE static void execute(int code, StackFrame* frame) {
        DECODE_2(BIT_W_VAR_ID, BIT_W_IP, testID, ip);
        auto test = VarID(testID).toValue(frame->localConstPtr);
        if (test->getBoolean()) frame->ip = ip - 1;
    }
    FORCE_INLINE static string disassemble(int code, FuncMeta* meta) {
        DECODE_2(BIT_W_VAR_ID, BIT_W_IP, testID, ip);
        return format("tjump %s,%d", VarID(testID).toString(meta).c_str(), ip + 1);
    }
};
template<>
struct ByteCodeHandler<BC_FalseJump> {
    FORCE_INLINE static void emitCode(int &code, int testID, int ip) {
        ENCODE_2(BC_FalseJump, BIT_W_VAR_ID, BIT_W_IP, testID, ip);
    }
    FORCE_INLINE static void execute(int code, StackFrame* frame) {
        DECODE_2(BIT_W_VAR_ID, BIT_W_IP, testID, ip);
        auto test = VarID(testID).toValue(frame->localConstPtr);
        if (!test->getBoolean()) frame->ip = ip - 1;
    }
    FORCE_INLINE static string disassemble(int code, FuncMeta* meta) {
        DECODE_2(BIT_W_VAR_ID, BIT_W_IP, testID, ip);
        return format("fjump %s,%d", VarID(testID).toString(meta).c_str(), ip + 1);
    }
};
template<>
struct ByteCodeHandler<BC_Call> {
    FORCE_INLINE static void emitCode(int &code, int funcID, int argID, int argCount) {
        ENCODE_3(BC_Call, BIT_W_VAR_ID, BIT_W_VAR_ID, 8, funcID, argID, argCount);
    }
    FORCE_INLINE static void execute(int code, StackFrame* frame) {
        DECODE_3(BIT_W_VAR_ID, BIT_W_VAR_ID, 8, funcID, argID, argCount);
        auto func = VarID(funcID).toValue(frame->localConstPtr);
        auto args = VarID(argID).toValue(frame->localConstPtr);
        ASSERT(func->type == JSVT_Function);
        func->data.func->callFromVM(args, args + argCount);
    }
    FORCE_INLINE static string disassemble(int code, FuncMeta* meta) {
        DECODE_3(BIT_W_VAR_ID, BIT_W_VAR_ID, 8, funcID, argID, argCount);
        return format("call %s,%s,%d", VarID(funcID).toString(meta).c_str(), VarID(argID).toString(meta).c_str(), argCount);
    }
};


#undef ENCODE_1
#undef ENCODE_2
#undef ENCODE_3
#undef DECODE_1
#undef DECODE_2
#undef DECODE_3

#endif
