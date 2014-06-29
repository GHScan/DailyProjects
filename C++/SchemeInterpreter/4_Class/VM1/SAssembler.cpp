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
}

template<typename T>
static T* emit(vector<uint8_t> &bytes, T v) {
    int off = (int)bytes.size();
    bytes.insert(bytes.end(), (uint8_t*)&v, (uint8_t*)&v + sizeof(v));
    return (T*)&bytes[off];
}

void SAssembler::assemble(vector<uint8_t> &bytes, SExpression e) {
    mFrees.clear();
    mLabels.clear();
    mLabelRefs.clear();

    for (auto free = e.getNode()->ref(1).getNode()->ref(1).getNode(); free != nullptr; free = free->next) {
        auto p = free->value.getNode()->ref(1).getNode();
        mFrees.push_back(make_pair(atoi(p->ref(0).getInt()->c_str()), atoi(p->ref(1).getInt()->c_str())));
    }

    for (auto code = e.getNode()->ref(2).getNode()->ref(1).getNode(); code != nullptr; code = code->next) {
        auto lcode = code->value.getNode();

        switch (lcode->ref(0).getSymbol()->getID()) {
            case SID_LoadK: {
                int kindex = atoi(lcode->ref(1).getInt()->c_str());
                SValue v = mConstants[kindex];
                if (v.getType() == SVT_Number) {
                    double n = v.getNumber();
                    if (floor(n) == n && n >= numeric_limits<int>::min() && n <= numeric_limits<int>::max()) {
                        emit(bytes, ByteCode<BCE_LoadInt>(int(n)));
                    } else {
                        emit(bytes, ByteCode<BCE_LoadDouble>(n));
                    }
                } else {
                    emit(bytes, ByteCode<BCE_LoadConstant>(kindex));
                }
            }
                break;
            case SID_LoadVar: {
                auto var = lcode->ref(1).getNode();
                switch (var->ref(0).getSymbol()->getID()) {
                    case SID_Local:
                        emit(bytes, ByteCode<BCE_LoadLocal>(atoi(var->ref(1).getInt()->c_str())));
                        break;
                    case SID_Global:
                        emit(bytes, ByteCode<BCE_LoadGlobal>(atoi(var->ref(1).getInt()->c_str())));
                        break;
                    case SID_Free: {
                         auto free = mFrees[atoi(var->ref(1).getInt()->c_str())];
                         if (free.first == 1) {
                             emit(bytes, ByteCode<BCE_LoadFree1>(free.second));
                         } else if (free.first == 2) {
                             emit(bytes, ByteCode<BCE_LoadFree2>(free.second));
                         } else if (free.first == 3) {
                             emit(bytes, ByteCode<BCE_LoadFree3>(free.second));
                         } else {
                             emit(bytes, ByteCode<BCE_LoadFree>(free.first, free.second));
                         }
                       }
                        break;
                    case SID_FreeMethod: {
                         auto free = mFrees[atoi(var->ref(1).getInt()->c_str())];
                         emit(bytes, ByteCode<BCE_LoadMethod>(free.first, free.second));
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
                        emit(bytes, ByteCode<BCE_StoreLocal>(atoi(var->ref(1).getInt()->c_str())));
                        break;
                    case SID_Global:
                        emit(bytes, ByteCode<BCE_StoreGlobal>(atoi(var->ref(1).getInt()->c_str())));
                        break;
                    case SID_Free: {
                         auto free = mFrees[atoi(var->ref(1).getInt()->c_str())];
                         if (free.first == 1) {
                             emit(bytes, ByteCode<BCE_StoreFree1>(free.second));
                         } else if (free.first == 2) {
                             emit(bytes, ByteCode<BCE_StoreFree2>(free.second));
                         } else if (free.first == 3) {
                             emit(bytes, ByteCode<BCE_StoreFree3>(free.second));
                         } else {
                             emit(bytes, ByteCode<BCE_StoreFree>(free.first, free.second));
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
                emit(bytes, ByteCode<BCE_LoadFunc>(atoi(lcode->ref(1).getInt()->c_str())));
                break;
            case SID_Pop:
                emit(bytes, ByteCode<BCE_Pop>());
                break;
            case SID_Label:
                mLabels.push_back(make_pair(lcode->ref(1).getSymbol(), (int)bytes.size()));
                break;
            case SID_Jmp:
                mLabelRefs.push_back(make_pair(lcode->ref(1).getSymbol(), emit(bytes, ByteCode<BCE_Jmp>())->getTargetPtr()));
                break;
            case SID_TJmp:
                mLabelRefs.push_back(make_pair(lcode->ref(1).getSymbol(), emit(bytes, ByteCode<BCE_TrueJmp>())->getTargetPtr()));
                break;
            case SID_Tail:
                emit(bytes, ByteCode<BCE_Tail>());
                break;
            case SID_Call:
                emit(bytes, ByteCode<BCE_Call>(atoi(lcode->ref(1).getInt()->c_str())));
                break;
            case SID_LoadClass:
                emit(bytes, ByteCode<BCE_LoadClass>(atoi(lcode->ref(1).getInt()->c_str())));
                break;
            case SID_GetField:
                emit(bytes, ByteCode<BCE_GetField>(atoi(lcode->ref(1).getNode()->ref(1).getInt()->c_str())));
                break;
            case SID_GetMethod:
                emit(bytes, ByteCode<BCE_GetMethod>(atoi(lcode->ref(1).getNode()->ref(1).getInt()->c_str())));
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
        checkedAssign(ref.second, iter->second);
    }
}
