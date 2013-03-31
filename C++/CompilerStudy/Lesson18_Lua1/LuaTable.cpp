#include "pch.h"

#include "LuaTable.h"
#include "Function.h"

static LuaValue invokeMeta(LuaTable* table, LuaTable *metaTable, const char* metaName, const LuaValue& arg1, const LuaValue& arg2) {
    LuaValue f = metaTable->get(LuaValue(metaName));
    vector<LuaValue> args, rets;
    table->addRef();
    args.push_back(LuaValue(table)); args.push_back(arg1); args.push_back(arg2);
    f.getFunction()->call(args, rets);
    return rets.empty() ? LuaValue::NIL : rets[0];
}

bool LuaTable::hasKey(const LuaValue& k) const {
    if (k.isTypeOf(LVT_Number)) {
        int idx = (int)k.getNumber() - 1;
        if (idx >= 0 && idx < (int)m_vec.size()) return true;
    } 
    return m_hashTable.find(k) != m_hashTable.end();
}
LuaValue LuaTable::get(const LuaValue& k, bool raw) const {
    if (k.isTypeOf(LVT_Number)) {
        int idx = (int)k.getNumber() - 1;
        if (idx >= 0 && idx < (int)m_vec.size()) return m_vec[idx];
    }
    auto iter = m_hashTable.find(k);
    if (iter != m_hashTable.end()) return iter->second;

    if (!raw && m_metaTable != NULL) {
        LuaValue f = m_metaTable->get(LuaValue("__index"));
        if (f.isTypeOf(LVT_Nil)) {}
        else if (f.isTypeOf(LVT_Table)) return f.getTable()->get(k);
        else if (f.isTypeOf(LVT_Function)) {
            vector<LuaValue> args, rets;
            {
                auto t = const_cast<LuaTable*>(this);
                t->addRef();
                args.push_back(LuaValue(t)); args.push_back(k);
            }
            f.getFunction()->call(args, rets);
            if (!rets.empty()) return rets[0];
        } else ASSERT(0);
    }

    return LuaValue::NIL;
}
void LuaTable::set(const LuaValue& k, const LuaValue& v, bool raw) {
    if (!raw && m_metaTable != NULL && !v.isTypeOf(LVT_Nil)) {
        if (!hasKey(k)) {
            LuaValue f = m_metaTable->get(LuaValue("__newindex"));
            if (f.isTypeOf(LVT_Nil)) {}
            else if (f.isTypeOf(LVT_Table)) {
                f.getTable()->set(k, v);
                return;
            } else if (f.isTypeOf(LVT_Function)) {
                vector<LuaValue> args, rets;
                addRef();
                args.push_back(LuaValue(this)); args.push_back(k); args.push_back(v);
                f.getFunction()->call(args, rets);
                return;
            } else ASSERT(0);
        }
    }

    if (k.isTypeOf(LVT_Number)) {
        int idx = (int)k.getNumber() - 1;
        if (idx >= 0 && idx < (int)m_vec.size()) {
            m_vec[idx] = v;
            return;
        }
        if (idx == (int)m_vec.size()) {
            m_vec.push_back(v);
            return;
        }
    }
    if (v.isTypeOf(LVT_Nil)) {
        m_hashTable.erase(k);
    } else m_hashTable[k] = v;
    return;
}
void LuaTable::arrayInsert(int off, const LuaValue& v) {
    ASSERT(off >= 0 && off <= (int)m_vec.size());
    m_vec.insert(m_vec.begin() + off, v);
}
LuaValue LuaTable::arrayRemove(int off) {
    ASSERT(off >= 0 && off < (int)m_vec.size());
    LuaValue r(m_vec[off]);
    m_vec.erase(m_vec.begin() + off);
    return r;
}

const LuaValue& LuaTable::getNext(LuaValue& k) const {
    if (k.isTypeOf(LVT_Nil)) {
        if (!m_vec.empty()) {
            k = LuaValue(NumberType(1));
            return m_vec.front();
        }
        if (!m_hashTable.empty()) {
            auto iter = m_hashTable.begin();
            k = iter->first;
            return iter->second;
        }
    }
    else {
        if (k.isTypeOf(LVT_Number)) {
            int idx = (int)k.getNumber();
            if (idx >= 0 && idx < (int)m_vec.size()) {
                k = LuaValue(NumberType(idx + 1));
                return m_vec[idx];
            }
            if (idx == (int)m_vec.size() && !m_hashTable.empty()) {
                auto iter = m_hashTable.begin();
                k = iter->first;
                return iter->second;
            }
        }
        auto iter = m_hashTable.find(k);
        if (iter != m_hashTable.end()) {
            ++iter;
            if (iter != m_hashTable.end()) {
                k = iter->first;
                return iter->second;
            }
        }
    }
    k = LuaValue::NIL;
    return LuaValue::NIL;
}
const LuaValue& LuaTable::getINext(LuaValue& k) const {
    if (k.isTypeOf(LVT_Nil)) {
        if (!m_vec.empty()) {
            k = LuaValue(NumberType(1));
            return m_vec.front();
        }
        return LuaValue::NIL;
    }
    else {
        int idx = (int)k.getNumber();
        if (idx >= 0 && idx < (int)m_vec.size()) {
            k = LuaValue(NumberType(idx + 1));
            return m_vec[idx];
        }
        k = LuaValue::NIL;
        return LuaValue::NIL;
    }
}
void LuaTable::sort() {
    std::sort(m_vec.begin(), m_vec.end());
}
void LuaTable::sort(const LuaValue& cmp) {
    auto func = cmp.getFunction();
    vector<LuaValue> args, rets;
    std::sort(m_vec.begin(), m_vec.end(), [func, &args, &rets](const LuaValue& a, const LuaValue& b){
        args.push_back(a); args.push_back(b);
        func->call(args, rets);
        bool r = rets[0].getBoolean(); 
        args.clear(); rets.clear();
        return r;
    });
}
LuaTable::LuaTable(): m_refCount(1), m_metaTable(NULL) {
}
LuaTable::~LuaTable() {
    if (m_metaTable != NULL) m_metaTable->releaseRef();
}
int LuaTable::releaseRef() const {
    if (m_refCount == 1 && m_metaTable != NULL) {
        // NOT STANDARD!
        invokeMeta(const_cast<LuaTable*>(this), m_metaTable, "__gc", LuaValue::NIL, LuaValue::NIL);
    }

    int r = --m_refCount;
    if (r == 0) delete this;
    return r;
}
void LuaTable::setMetaTable(LuaTable *t) {
    if (m_metaTable != t) {
        if (m_metaTable != NULL) m_metaTable->releaseRef();
        if (t != NULL) t->addRef();
        m_metaTable = t;
    }
}
LuaValue LuaTable::meta_add(const LuaValue& v) {
    return invokeMeta(this, m_metaTable, "__add", v, LuaValue::NIL);
}
LuaValue LuaTable::meta_sub(const LuaValue& v) {
    return invokeMeta(this, m_metaTable, "__sub", v, LuaValue::NIL);
}
LuaValue LuaTable::meta_mul(const LuaValue& v) {
    return invokeMeta(this, m_metaTable, "__mul", v, LuaValue::NIL);
}
LuaValue LuaTable::meta_div(const LuaValue& v) {
    return invokeMeta(this, m_metaTable, "__div", v, LuaValue::NIL);
}
LuaValue LuaTable::meta_mod(const LuaValue& v) {
    return invokeMeta(this, m_metaTable, "__mod", v, LuaValue::NIL);
}
LuaValue LuaTable::meta_pow(const LuaValue& v) {
    return invokeMeta(this, m_metaTable, "__pow", v, LuaValue::NIL);
}
LuaValue LuaTable::meta_concat(const LuaValue& v) {
    return invokeMeta(this, m_metaTable, "__concat", v, LuaValue::NIL);
}
LuaValue LuaTable::meta_eq(const LuaValue& v) {
    return invokeMeta(this, m_metaTable, "__eq", v, LuaValue::NIL);
}
LuaValue LuaTable::meta_lt(const LuaValue& v) {
    return invokeMeta(this, m_metaTable, "__lt", v, LuaValue::NIL);
}
LuaValue LuaTable::meta_le(const LuaValue& v) {
    return invokeMeta(this, m_metaTable, "__le", v, LuaValue::NIL);
}
LuaValue LuaTable::meta_unm() {
    return invokeMeta(this, m_metaTable, "__unm", LuaValue::NIL, LuaValue::NIL);
}
void LuaTable::meta_call(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    LuaValue f = m_metaTable->get(LuaValue("__call"));
    vector<LuaValue> _args;
    addRef();
    _args.push_back(LuaValue(this)); 
    _args.insert(_args.end(), args.begin(), args.end());
    f.getFunction()->call(_args, rets);
}
bool LuaTable::hasMeta(const char* metaName) {
    return m_metaTable != NULL && !m_metaTable->get(LuaValue(metaName)).isTypeOf(LVT_Nil);
}
