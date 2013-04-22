
#ifndef BYTE_CODE_DEFINE_H
#define BYTE_CODE_DEFINE_H

enum ByteCode {
    BC_PushLocal, BC_PushUpValue, BC_PushGlobal,
    BC_PushConst,
    BC_PushVArgs,
    BC_PushNewFunction, BC_PushNewTable,
    BC_PushI, BC_PushTop,

    BC_PopLocal, BC_PopUpValue, BC_PopGlobal,
    BC_PopN, BC_PopTemps,

    BC_ResizeTemp, 

    BC_Call, 
    BC_CloseBlock,

    BC_Less, BC_LessEq, BC_Greater, BC_GreaterEq, BC_Equal, BC_NEqual,
    BC_Add, BC_Sub, BC_Mul,
    BC_Div, BC_Mod, BC_Pow,
    BC_Concat,

    BC_Not, BC_Len, BC_Minus,

    BC_GetTable, BC_SetTable, BC_PushAll2Table,

    BC_Jump,
    BC_TrueJump, BC_FalseJump,

    BC_Nop,
};

template<int n>
struct ByteCodeHandler;

template<>
struct ByteCodeHandler<BC_PushLocal> {
    static void emit(int &code, int localIdx) {
        code = BC_PushLocal | (localIdx) << 8;
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        so << format("pushLocal %d", code >> 8);
    }
    static void execute(int code, LuaStackFrame* frame) {
        frame->pushTemp(frame->local(code >> 8));
    }
};
template<>
struct ByteCodeHandler<BC_PushUpValue> {
    static void emit(int &code, int uvIdx) {
        code = BC_PushUpValue | (uvIdx) << 8;
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        so << format("pushUpValue %d", code >> 8);
    }
    static void execute(int code, LuaStackFrame* frame) {
        frame->pushTemp(static_cast<LuaFunction*>(frame->func)->upValue(code >> 8));
    }
};
template<>
struct ByteCodeHandler<BC_PushGlobal> {
    static void emit(int &code, int constIdx) {
        code = BC_PushGlobal | (constIdx) << 8;
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        so << format("pushGlobal %s", meta->constTable[code >> 8].getString()->buf());
    }
    static void execute(int code, LuaStackFrame* frame) {
        auto lfunc = static_cast<LuaFunction*>(frame->func);
        LuaValue &k = lfunc->meta->constTable[code >> 8];
        frame->pushTemp(lfunc->fenvTable->get(k));
    }
};
template<>
struct ByteCodeHandler<BC_PushConst> {
    static void emit(int &code, int constIdx) {
        code = BC_PushConst | (constIdx) << 8;
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        so << format("pushConst %s", meta->constTable[code >> 8].toString().c_str());
    }
    static void execute(int code, LuaStackFrame* frame) {
        frame->pushTemp(static_cast<LuaFunction*>(frame->func)->meta->constTable[code >> 8]);
    }
};
template<>
struct ByteCodeHandler<BC_PushVArgs> {
    static void emit(int &code) {
        code = BC_PushVArgs;
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        so << format("pushVArgs");
    }
    static void execute(int code, LuaStackFrame* frame) {
        if (frame->varParamBase < frame->localBase) {
            frame->pushTemp(frame->stackValue(frame->varParamBase));
            for (int i = frame->varParamBase + 1; i < frame->localBase; ++i) {
                frame->pushExtTemp(frame->stackValue(i));
            }
        } else {
            frame->pushTemp(LuaValue::NIL);
        }
    }
};
template<>
struct ByteCodeHandler<BC_PushNewFunction> {
    static void emit(int &code, int metaIdx) {
        code = BC_PushNewFunction | (metaIdx) << 8;
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        so << format("pushNewfunction %d", code >> 8);
    }
    static void execute(int code, LuaStackFrame* frame) {
        frame->pushTemp(LuaValue(LuaFunction::create(LuaVM::instance()->getMeta(code >> 8))));
    }
};
template<>
struct ByteCodeHandler<BC_PushNewTable> {
    static void emit(int &code) {
        code = BC_PushNewTable;
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        so << format("pushNewTable");
    }
    static void execute(int code, LuaStackFrame* frame) {
        frame->pushTemp(LuaValue(LuaTable::create()));
    }
};
template<>
struct ByteCodeHandler<BC_PushI> {
    static void emit(int &code, int idx) {
        code = BC_PushI | (idx) << 8;
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        so << format("pushI %d", code >> 8);
    }
    static void execute(int code, LuaStackFrame* frame) {
        frame->pushTemp(frame->temp(code >> 8));
    }
};
template<>
struct ByteCodeHandler<BC_PushTop> {
    static void emit(int &code, int idx) {
        code = BC_PushTop | (idx) << 8;
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        so << format("pushTop %d", code >> 8);
    }
    static void execute(int code, LuaStackFrame* frame) {
        frame->pushTemp(frame->topTemp(code >> 8));
    }
};
template<>
struct ByteCodeHandler<BC_PopLocal> {
    static void emit(int &code, int localIdx) {
        code = BC_PopLocal | (localIdx) << 8;
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        so << format("popLocal %d", code >> 8);
    }
    static void execute(int code, LuaStackFrame* frame) {
        frame->local(code >> 8) = frame->topTemp(0);
        frame->popTempN(1);
    }
};
template<>
struct ByteCodeHandler<BC_PopUpValue> {
    static void emit(int &code, int uvIdx) {
        code = BC_PopUpValue | (uvIdx) << 8;
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        so << format("popUpValue %d", code >> 8);
    }
    static void execute(int code, LuaStackFrame* frame) {
        static_cast<LuaFunction*>(frame->func)->upValue(code >> 8) = frame->topTemp(0);
        frame->popTempN(1);
    }
};
template<>
struct ByteCodeHandler<BC_PopGlobal> {
    static void emit(int &code, int constIdx) {
        code = BC_PopGlobal | (constIdx) << 8;
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        so << format("popGlobal %s", meta->constTable[code >> 8].getString()->buf());
    }
    static void execute(int code, LuaStackFrame* frame) {
        auto lfunc = static_cast<LuaFunction*>(frame->func);
        LuaValue &k = lfunc->meta->constTable[code >> 8];
        lfunc->fenvTable->set(k, frame->topTemp(0));
        frame->popTempN(1);
    }
};
template<>
struct ByteCodeHandler<BC_PopN> {
    static void emit(int &code, int n) {
        code = BC_PopN | (n) << 8;
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        so << format("popN %d", code >> 8);
    }
    static void execute(int code, LuaStackFrame* frame) {
        frame->popTempN(code >> 8);
    }
};
template<>
struct ByteCodeHandler<BC_PopTemps> {
    static void emit(int &code) {
        code = BC_PopTemps;
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        so << format("popTemps");
    }
    static void execute(int code, LuaStackFrame* frame) {
        frame->popTemps(0);
    }
};
template<>
struct ByteCodeHandler<BC_ResizeTemp> {
    static void emit(int &code, int n) {
        code = BC_ResizeTemp | (n) << 8;
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        so << format("resizeTemp %d", code >> 8);
    }
    static void execute(int code, LuaStackFrame* frame) {
        frame->resizeTemp(code >> 8);
    }
};
template<>
struct ByteCodeHandler<BC_Call> {
    static void emit(int &code, int paramCount) {
        code = BC_Call | (paramCount) << 8;
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        so << format("call %d", code >> 8);
    }
    static void execute(int code, LuaStackFrame* frame) {
        int tempIdx = frame->tempCount - (code >> 8) - 1;
        if (frame->temp(tempIdx).isTypeOf(LVT_Table)) {
            frame->temp(tempIdx).getTable()->meta_call(tempIdx, frame);
        } else {
            callFunc(tempIdx);
        }
    }
};
template<>
struct ByteCodeHandler<BC_CloseBlock> {
    static void emit(int &code, int localOff, int localCount) {
        ASSERT(localCount < (1 << 8) && localOff < (1 << 16));
        code = BC_CloseBlock | (localOff << 8 | localCount) << 8;
    }
    static void emitOff(int &code, int localOff) {
        code = (code & 0xffff) | (localOff << 16);
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        code >>= 8;
        so << format("closeblock %d,%d", code >> 8, code & 0xff);
    }
    static void execute(int code, LuaStackFrame* frame) {
        code >>= 8;
        int localOff = code >> 8, localCount = code & 0xff;
        auto& closures = frame->closures;
        auto iter = closures.lower_bound(localOff);
        while (iter != closures.end()) {
            assert(iter->first >= localOff && iter->first < localOff + localCount);
            auto iter2 = iter;
            do {
                ++iter2;
            } while(iter2 != closures.end() && iter2->first == iter->first);
            shared_ptr<LuaValue> v(new LuaValue(frame->local(iter->first)));
            for (; iter != iter2; ++iter) {
                auto& funcUvIdx = iter->second;
                funcUvIdx.first->sharedUpValues.push_back(v);
                funcUvIdx.first->upValues[funcUvIdx.second] = v.get();
            }
        }
    }
};
template<>
struct ByteCodeHandler<BC_Less> {
    static void emit(int &code) {
        code = BC_Less;
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        so << format("less");
    }
    static void execute(int code, LuaStackFrame* frame) {
        frame->topTemp(-1) = frame->topTemp(-1) < frame->topTemp(0) ? LuaValue::TRUE : LuaValue::FALSE;
        frame->popTempN(1);
    }
};
template<>
struct ByteCodeHandler<BC_LessEq> {
    static void emit(int &code) {
        code = BC_LessEq;
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        so << format("lessEq");
    }
    static void execute(int code, LuaStackFrame* frame) {
        frame->topTemp(-1) = frame->topTemp(-1) <= frame->topTemp(0) ? LuaValue::TRUE : LuaValue::FALSE;
        frame->popTempN(1);
    }
};
template<>
struct ByteCodeHandler<BC_Greater> {
    static void emit(int &code) {
        code = BC_Greater;
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        so << format("greater");
    }
    static void execute(int code, LuaStackFrame* frame) {
        frame->topTemp(-1) = frame->topTemp(-1) > frame->topTemp(0) ? LuaValue::TRUE : LuaValue::FALSE;
        frame->popTempN(1);
    }
};
template<>
struct ByteCodeHandler<BC_GreaterEq> {
    static void emit(int &code) {
        code = BC_GreaterEq;
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        so << format("greaterEq");
    }
    static void execute(int code, LuaStackFrame* frame) {
        frame->topTemp(-1) = frame->topTemp(-1) >= frame->topTemp(0) ? LuaValue::TRUE : LuaValue::FALSE;
        frame->popTempN(1);
    }
};
template<>
struct ByteCodeHandler<BC_Equal> {
    static void emit(int &code) {
        code = BC_Equal;
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        so << format("equal");
    }
    static void execute(int code, LuaStackFrame* frame) {
        frame->topTemp(-1) = frame->topTemp(-1) == frame->topTemp(0) ? LuaValue::TRUE : LuaValue::FALSE;
        frame->popTempN(1);
    }
};
template<>
struct ByteCodeHandler<BC_NEqual> {
    static void emit(int &code) {
        code = BC_NEqual;
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        so << format("nequal");
    }
    static void execute(int code, LuaStackFrame* frame) {
        frame->topTemp(-1) = frame->topTemp(-1) != frame->topTemp(0) ? LuaValue::TRUE : LuaValue::FALSE;
        frame->popTempN(1);
    }
};
template<>
struct ByteCodeHandler<BC_Add> {
    static void emit(int &code) {
        code = BC_Add;
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        so << format("add");
    }
    static void execute(int code, LuaStackFrame* frame) {
        auto& lv = frame->topTemp(-1);
        lv = lv + frame->topTemp(0);
        frame->popTempN(1);
    }
};
template<>
struct ByteCodeHandler<BC_Sub> {
    static void emit(int &code) {
        code = BC_Sub;
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        so << format("sub");
    }
    static void execute(int code, LuaStackFrame* frame) {
        auto& lv = frame->topTemp(-1);
        lv = lv - frame->topTemp(0);
        frame->popTempN(1);
    }
};
template<>
struct ByteCodeHandler<BC_Mul> {
    static void emit(int &code) {
        code = BC_Mul;
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        so << format("mul");
    }
    static void execute(int code, LuaStackFrame* frame) {
        auto& lv = frame->topTemp(-1);
        lv = lv * frame->topTemp(0);
        frame->popTempN(1);
    }
};
template<>
struct ByteCodeHandler<BC_Div> {
    static void emit(int &code) {
        code = BC_Div;
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        so << format("div");
    }
    static void execute(int code, LuaStackFrame* frame) {
        auto& lv = frame->topTemp(-1);
        lv = lv / frame->topTemp(0);
        frame->popTempN(1);
    }
};
template<>
struct ByteCodeHandler<BC_Mod> {
    static void emit(int &code) {
        code = BC_Mod;
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        so << format("mod");
    }
    static void execute(int code, LuaStackFrame* frame) {
        auto& lv = frame->topTemp(-1);
        lv = lv % frame->topTemp(0);
        frame->popTempN(1);
    }
};
template<>
struct ByteCodeHandler<BC_Pow> {
    static void emit(int &code) {
        code = BC_Pow;
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        so << format("pow");
    }
    static void execute(int code, LuaStackFrame* frame) {
        auto& lv = frame->topTemp(-1);
        lv = power(lv, frame->topTemp(0));
        frame->popTempN(1);
    }
};
template<>
struct ByteCodeHandler<BC_Concat> {
    static void emit(int &code) {
        code = BC_Concat;
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        so << format("concat");
    }
    static void execute(int code, LuaStackFrame* frame) {
        auto& lv = frame->topTemp(-1);
        lv = concat(lv, frame->topTemp(0));
        frame->popTempN(1);
    }
};
template<>
struct ByteCodeHandler<BC_Not> {
    static void emit(int &code) {
        code = BC_Not;
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        so << format("not");
    }
    static void execute(int code, LuaStackFrame* frame) {
        frame->topTemp(0) = frame->topTemp(0).getBoolean() ? LuaValue::FALSE : LuaValue::TRUE;
    }
};
template<>
struct ByteCodeHandler<BC_Len> {
    static void emit(int &code) {
        code = BC_Len;
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        so << format("len");
    }
    static void execute(int code, LuaStackFrame* frame) {
        frame->topTemp(0) = LuaValue(frame->topTemp(0).getSize());
    }
};
template<>
struct ByteCodeHandler<BC_Minus> {
    static void emit(int &code) {
        code = BC_Minus;
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        so << format("minus");
    }
    static void execute(int code, LuaStackFrame* frame) {
        auto &v = frame->topTemp(0);
        if (v.isTypeOf(LVT_Table)) v = v.getTable()->meta_unm();
        else v = LuaValue(-v.getNumber());
    }
};
template<>
struct ByteCodeHandler<BC_GetTable> {
    static void emit(int &code) {
        code = BC_GetTable;
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        so << format("get");
    }
    static void execute(int code, LuaStackFrame* frame) {
        frame->topTemp(-1) = frame->topTemp(-1).getTable()->get(frame->topTemp(0));
        frame->popTempN(1);
    }
};
template<>
struct ByteCodeHandler<BC_SetTable> {
    static void emit(int &code) {
        code = BC_SetTable;
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        so << format("set");
    }
    static void execute(int code, LuaStackFrame* frame) {
        frame->topTemp(-2).getTable()->set(frame->topTemp(-1), frame->topTemp(0));
        frame->popTempN(3);
    }
};
template<>
struct ByteCodeHandler<BC_PushAll2Table> {
    static void emit(int &code, int count) {
        code = BC_PushAll2Table | (count) << 8;
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        so << format("pushAll2Table %d", code >> 8);
    }
    static void execute(int code, LuaStackFrame* frame) {
        int tempIdx = frame->tempCount - (code >> 8) - 1;
        auto table = frame->temp(tempIdx).getTable();
        for (int i = tempIdx + 1; i < frame->tempExtCount; ++i) {
            table->arrayInsert(table->size(), frame->temp(i));
        }
        frame->popTemps(tempIdx);
    }
};
template<>
struct ByteCodeHandler<BC_Jump> {
    static void emit(int &code, int ip) {
        code = BC_Jump | (ip) << 8;
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        so << format("jump %d", (code >> 8) + 1);
    }
    static void execute(int code, LuaStackFrame* frame) {
        frame->ip = (code >> 8) - 1;
    }
};
template<>
struct ByteCodeHandler<BC_TrueJump> {
    static void emit(int &code, int ip) {
        code = BC_TrueJump | (ip) << 8;
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        so << format("tjump %d", (code >> 8) + 1);
    }
    static void execute(int code, LuaStackFrame* frame) {
        if (frame->topTemp(0).getBoolean()) {
            frame->ip = (code >> 8) - 1;
        }
        frame->popTempN(1);
    }
};
template<>
struct ByteCodeHandler<BC_FalseJump> {
    static void emit(int &code, int ip) {
        code = BC_FalseJump | (ip) << 8;
    }
    static void disassemble(ostream& so, int code, LuaFunctionMeta* meta) {
        so << format("fjump %d", (code >> 8) + 1);
    }
    static void execute(int code, LuaStackFrame* frame) {
        if (!frame->topTemp(0).getBoolean()) {
            frame->ip = (code >> 8) - 1;
        }
        frame->popTempN(1);
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

#endif
