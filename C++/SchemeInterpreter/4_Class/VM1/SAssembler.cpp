#include "pch.h"
#include "SAssembler.h"
#include "Atom.h"
#include "ByteCodeDefinition.h"

namespace  {

enum SymbolID {
    SID_LoadK = 1,
    SID_LoadVar,
    SID_StoreVar,
    SID_LoadFunc,
    SID_Pop,
    SID_Jmp,
    SID_TJmp,
    SID_OpJmp,
    SID_Tail,
    SID_Call,
    SID_LoadClass,
    SID_GetField,
    SID_GetMethod,
    SID_Label,
    SID_Local,
    SID_Global,
    SID_Free,
    SID_FreeMethod,
    SID_InlineOp,
};

}

SAssembler::SAssembler(const vector<SValue> &constants):
    mConstants(constants) {

    ASSERT(AtomPool::instance()->size() == 0);

    AtomPool::instance()->intern("loadk", SID_LoadK);
    AtomPool::instance()->intern("loadvar", SID_LoadVar);
    AtomPool::instance()->intern("storevar", SID_StoreVar);
    AtomPool::instance()->intern("loadfunc", SID_LoadFunc);
    AtomPool::instance()->intern("pop", SID_Pop);
    AtomPool::instance()->intern("jmp", SID_Jmp);
    AtomPool::instance()->intern("tjmp", SID_TJmp);
    AtomPool::instance()->intern("opjmp", SID_OpJmp);
    AtomPool::instance()->intern("tail", SID_Tail);
    AtomPool::instance()->intern("call", SID_Call);
    AtomPool::instance()->intern("loadclass", SID_LoadClass);
    AtomPool::instance()->intern("getfield", SID_GetField);
    AtomPool::instance()->intern("getmethod", SID_GetMethod);
    AtomPool::instance()->intern("local", SID_Local);
    AtomPool::instance()->intern("global", SID_Global);
    AtomPool::instance()->intern("free", SID_Free);
    AtomPool::instance()->intern("free-method", SID_FreeMethod);
    AtomPool::instance()->intern("label", SID_Label);
    AtomPool::instance()->intern("inline-op", SID_InlineOp);
}

template<typename T>
static T* emit(vector<uint8_t> &codes, T v) {
    int off = (int)codes.size();
    codes.insert(codes.end(), (uint8_t*)&v, (uint8_t*)&v + sizeof(v));
    return (T*)&codes[off];
}

void SAssembler::assemble(vector<uint8_t> &codes, SExpression e) {
    mFrees.clear();
    mLabels.clear();
    mLabelRefs.clear();

    for (auto free = e.getNode()->ref(2).getNode()->ref(1).getNode(); free != nullptr; free = free->next) {
        auto p = free->value.getNode()->ref(1).getNode();
        mFrees.push_back(make_pair(atoi(p->ref(0).getInt()->c_str()), atoi(p->ref(1).getInt()->c_str())));
    }

    for (auto code = e.getNode()->ref(3).getNode()->ref(1).getNode(); code != nullptr; code = code->next) {
        auto lcode = code->value.getNode();

        switch (lcode->ref(0).getSymbol()->getID()) {
            case SID_LoadK: {
                int kindex = atoi(lcode->ref(1).getInt()->c_str());
                SValue v = mConstants[kindex];
                if (v.getType() == SVT_Number) {
                    double n = v.getNumber();
                    if (floor(n) == n && n >= numeric_limits<int>::min() && n <= numeric_limits<int>::max()) {
                        emit(codes, ByteCode<BCE_LoadInt>(int(n)));
                    } else {
                        emit(codes, ByteCode<BCE_LoadDouble>(n));
                    }
                } else {
                    emit(codes, ByteCode<BCE_LoadConstant>(kindex));
                }
            }
                break;
            case SID_LoadVar: {
                auto var = lcode->ref(1).getNode();
                switch (var->ref(0).getSymbol()->getID()) {
                    case SID_Local: {
                            int lindex = atoi(var->ref(1).getInt()->c_str());
                            switch (lindex) {
                                case 0: emit(codes, ByteCode<BCE_LoadLocal0>()); break;
                                case 1: emit(codes, ByteCode<BCE_LoadLocal1>()); break;
                                case 2: emit(codes, ByteCode<BCE_LoadLocal2>()); break;
                                default: emit(codes, ByteCode<BCE_LoadLocal>(lindex)); break;
                            }
                        }
                        break;
                    case SID_Global:
                        emit(codes, ByteCode<BCE_LoadGlobal>(atoi(var->ref(1).getInt()->c_str())));
                        break;
                    case SID_Free: {
                         auto free = mFrees[atoi(var->ref(1).getInt()->c_str())];
                         if (free.first == 1) {
                             emit(codes, ByteCode<BCE_LoadFree1>(free.second));
                         } else if (free.first == 2) {
                             emit(codes, ByteCode<BCE_LoadFree2>(free.second));
                         } else if (free.first == 3) {
                             emit(codes, ByteCode<BCE_LoadFree3>(free.second));
                         } else {
                             emit(codes, ByteCode<BCE_LoadFree>(free.first, free.second));
                         }
                       }
                        break;
                    case SID_FreeMethod: {
                         auto free = mFrees[atoi(var->ref(1).getInt()->c_str())];
                         emit(codes, ByteCode<BCE_LoadMethod>(free.first, free.second));
                     }
                        break;
                    default:
                        ASSERT(0);
                        break;
                }
              }
                break;
            case SID_StoreVar: {
                auto var = lcode->ref(1).getNode();
                switch (var->ref(0).getSymbol()->getID()) {
                    case SID_Local:
                        emit(codes, ByteCode<BCE_StoreLocal>(atoi(var->ref(1).getInt()->c_str())));
                        break;
                    case SID_Global:
                        emit(codes, ByteCode<BCE_StoreGlobal>(atoi(var->ref(1).getInt()->c_str())));
                        break;
                    case SID_Free: {
                         auto free = mFrees[atoi(var->ref(1).getInt()->c_str())];
                         if (free.first == 1) {
                             emit(codes, ByteCode<BCE_StoreFree1>(free.second));
                         } else if (free.first == 2) {
                             emit(codes, ByteCode<BCE_StoreFree2>(free.second));
                         } else if (free.first == 3) {
                             emit(codes, ByteCode<BCE_StoreFree3>(free.second));
                         } else {
                             emit(codes, ByteCode<BCE_StoreFree>(free.first, free.second));
                         }
                       }
                        break;
                    default:
                        ASSERT(0);
                        break;
                }
               }
                break;
            case SID_LoadFunc: 
                emit(codes, ByteCode<BCE_LoadFunc>(atoi(lcode->ref(1).getInt()->c_str())));
                break;
            case SID_Pop:
                emit(codes, ByteCode<BCE_Pop>());
                break;
            case SID_Label:
                mLabels.push_back(make_pair(lcode->ref(1).getSymbol(), (int)codes.size()));
                break;
            case SID_Jmp:
                mLabelRefs.push_back(make_pair(lcode->ref(1).getSymbol(), (uint8_t*)emit(codes, ByteCode<BCE_Jmp>())->getTargetPtr() - &codes[0]));
                break;
            case SID_TJmp:
                mLabelRefs.push_back(make_pair(lcode->ref(1).getSymbol(), (uint8_t*)emit(codes, ByteCode<BCE_TrueJmp>())->getTargetPtr() - &codes[0]));
                break;
            case SID_OpJmp: {
                    if (strcmp(lcode->ref(2).getSymbol()->c_str(), "not") == 0) {
                        mLabelRefs.push_back(make_pair(lcode->ref(1).getSymbol(), (uint8_t*)emit(codes, ByteCode<BCE_FalseJmp>())->getTargetPtr() - &codes[0]));
                    } else if (strcmp(lcode->ref(2).getSymbol()->c_str(), "=") == 0) {
                        mLabelRefs.push_back(make_pair(lcode->ref(1).getSymbol(), (uint8_t*)emit(codes, ByteCode<BCE_Num_EqualJmp>())->getTargetPtr() - &codes[0]));
                    } else if (strcmp(lcode->ref(2).getSymbol()->c_str(), "<") == 0) {
                        mLabelRefs.push_back(make_pair(lcode->ref(1).getSymbol(), (uint8_t*)emit(codes, ByteCode<BCE_Num_LessJmp>())->getTargetPtr() - &codes[0]));
                    } else if (strcmp(lcode->ref(2).getSymbol()->c_str(), "<=") == 0) {
                        mLabelRefs.push_back(make_pair(lcode->ref(1).getSymbol(), (uint8_t*)emit(codes, ByteCode<BCE_Num_LessEqJmp>())->getTargetPtr() - &codes[0]));
                    } else if (strcmp(lcode->ref(2).getSymbol()->c_str(), ">") == 0) {
                        mLabelRefs.push_back(make_pair(lcode->ref(1).getSymbol(), (uint8_t*)emit(codes, ByteCode<BCE_Num_GreaterJmp>())->getTargetPtr() - &codes[0]));
                    } else if (strcmp(lcode->ref(2).getSymbol()->c_str(), ">=") == 0) {
                        mLabelRefs.push_back(make_pair(lcode->ref(1).getSymbol(), (uint8_t*)emit(codes, ByteCode<BCE_Num_GreaterEqJmp>())->getTargetPtr() - &codes[0]));
                    } else if (strcmp(lcode->ref(2).getSymbol()->c_str(), "eq?") == 0) {
                        mLabelRefs.push_back(make_pair(lcode->ref(1).getSymbol(), (uint8_t*)emit(codes, ByteCode<BCE_EqJmp>())->getTargetPtr() - &codes[0]));
                    } else if (strcmp(lcode->ref(2).getSymbol()->c_str(), "empty?") == 0) {
                        mLabelRefs.push_back(make_pair(lcode->ref(1).getSymbol(), (uint8_t*)emit(codes, ByteCode<BCE_EmptyJmp>())->getTargetPtr() - &codes[0]));
                    } else {
                        ASSERT(0);
                    }
                }
                break;
            case SID_Tail:
                emit(codes, ByteCode<BCE_Tail>());
                break;
            case SID_Call:
                emit(codes, ByteCode<BCE_Call>(atoi(lcode->ref(1).getInt()->c_str())));
                break;
            case SID_LoadClass:
                emit(codes, ByteCode<BCE_LoadClass>(atoi(lcode->ref(1).getInt()->c_str())));
                break;
            case SID_GetField:
                emit(codes, ByteCode<BCE_GetField>(atoi(lcode->ref(1).getNode()->ref(1).getInt()->c_str())));
                break;
            case SID_GetMethod:
                emit(codes, ByteCode<BCE_GetMethod>(atoi(lcode->ref(1).getNode()->ref(1).getInt()->c_str())));
                break;
            case SID_InlineOp: {
                if (strcmp(lcode->ref(1).getSymbol()->c_str(), "+") == 0) {
                    emit(codes, ByteCode<BCE_Inline_Add>());
                } else if (strcmp(lcode->ref(1).getSymbol()->c_str(), "-") == 0) {
                    emit(codes, ByteCode<BCE_Inline_Sub>());
                } else if (strcmp(lcode->ref(1).getSymbol()->c_str(), "*") == 0) {
                    emit(codes, ByteCode<BCE_Inline_Mul>());
                } else if (strcmp(lcode->ref(1).getSymbol()->c_str(), "/") == 0) {
                    emit(codes, ByteCode<BCE_Inline_Div>());
                } else if (strcmp(lcode->ref(1).getSymbol()->c_str(), "quotient") == 0) {
                    emit(codes, ByteCode<BCE_Inline_Quotient>());
                } else if (strcmp(lcode->ref(1).getSymbol()->c_str(), "remainder") == 0) {
                    emit(codes, ByteCode<BCE_Inline_Mod>());
                } else if (strcmp(lcode->ref(1).getSymbol()->c_str(), "=") == 0) {
                    emit(codes, ByteCode<BCE_Inline_Num_Equal>());
                } else if (strcmp(lcode->ref(1).getSymbol()->c_str(), "<") == 0) {
                    emit(codes, ByteCode<BCE_Inline_Num_Less>());
                } else if (strcmp(lcode->ref(1).getSymbol()->c_str(), "<=") == 0) {
                    emit(codes, ByteCode<BCE_Inline_Num_LessEq>());
                } else if (strcmp(lcode->ref(1).getSymbol()->c_str(), ">") == 0) {
                    emit(codes, ByteCode<BCE_Inline_Num_Greater>());
                } else if (strcmp(lcode->ref(1).getSymbol()->c_str(), ">=") == 0) {
                    emit(codes, ByteCode<BCE_Inline_Num_GreaterEq>());
                } else if (strcmp(lcode->ref(1).getSymbol()->c_str(), "not") == 0) {
                    emit(codes, ByteCode<BCE_Inline_Not>());
                } else if (strcmp(lcode->ref(1).getSymbol()->c_str(), "cons") == 0) {
                    emit(codes, ByteCode<BCE_Inline_Cons>());
                } else if (strcmp(lcode->ref(1).getSymbol()->c_str(), "car") == 0) {
                    emit(codes, ByteCode<BCE_Inline_Car>());
                } else if (strcmp(lcode->ref(1).getSymbol()->c_str(), "cdr") == 0) {
                    emit(codes, ByteCode<BCE_Inline_Cdr>());
                } else if (strcmp(lcode->ref(1).getSymbol()->c_str(), "eq?") == 0) {
                    emit(codes, ByteCode<BCE_Inline_Eq>());
                } else if (strcmp(lcode->ref(1).getSymbol()->c_str(), "empty?") == 0) {
                    emit(codes, ByteCode<BCE_Inline_Empty>());
                } else {
                    ASSERT(0);
                }
               }
                break;
            default:
                ASSERT(0);
                break;
        }
    }

    auto comparer = [](const pair<Atom*, int> &a, const pair<Atom*, int> &b){ return a.first < b.first;};
    sort(mLabels.begin(), mLabels.end(), comparer);

    for (auto ref : mLabelRefs) {
        auto iter = lower_bound(mLabels.begin(), mLabels.end(), make_pair(ref.first, 0), comparer);
        checkedAssign((uint16_t*)&codes[ref.second], iter->second);
    }
}
