
#include "pch.h"
#include "LuaTable.h"
#include "LuaFunction.h"
#include "LuaVM.h"
#include "GCObject.h"
#include "LuaStack.h"

static LuaValue invokeMeta(LuaTable* table, LuaValue& func, const LuaValue& arg0, const LuaValue& arg1 = LuaValue::NIL) {
    vector<LuaValue> params, rets;
    params.push_back(LuaValue(table));
    params.push_back(arg0);
    params.push_back(arg1);
    callFunc(func, params, rets);
    return rets.empty() ? LuaValue::NIL : rets[0];
}
static LuaValue invokeMeta(LuaTable* table, LuaTable *metaTable, const char *metaName, const LuaValue& arg0, const LuaValue& arg1 = LuaValue::NIL) {
    LuaValue func = metaTable->get(LuaValue(metaName));
    return invokeMeta(table, func, arg0, arg1);
}

LuaTable::LuaTable(): GCObject(OT_Table), m_metaTable(NULL) {
    LuaVM::instance()->getGCObjManager()->linkObject(this);
}
LuaTable::~LuaTable() {
}

LuaValue LuaTable::get(const LuaValue& k, bool raw) {
    if (k.isTypeOf(LVT_Number)) {
        int idx = (int)k.getNumber() - 1;
        if (idx >= 0 && idx < (int)m_array.size()) return m_array[idx];
    }
    auto iter = m_dict.find(k);
    if (iter != m_dict.end()) return iter->second;

    if (!raw) {
        LuaValue m = getMeta("__index");
        if (m.isNil());
        else if (m.isTypeOf(LVT_Table)) {
            return m.getTable()->get(k);
        } else if (m.isTypeOf(LVT_Function)) {
            return invokeMeta(this, m, k);
        } else ;
    }

    return LuaValue::NIL;
}
void LuaTable::set(const LuaValue& k, const LuaValue& v, bool raw) {
    if (!raw && m_metaTable != NULL && get(k, true).isNil()) {
        LuaValue m = getMeta("__newindex");
        if (m.isNil());
        else if (m.isTypeOf(LVT_Table)) {
            m.getTable()->set(k, v);
            return;
        } else if (m.isTypeOf(LVT_Function)) {
            invokeMeta(this, m, k, v);
            return;
        } else ;
    }

    if (k.isTypeOf(LVT_Number)) {
        int idx = (int)k.getNumber() - 1;
        if (idx >= 0 && idx < (int)m_array.size()) {
            if (idx == (int)m_array.size() - 1 && v.isNil()) {
                m_array.pop_back();
            } else {
                m_array[idx] = v;
            }
            return;
        } else if (idx == (int)m_array.size()) {
            m_array.push_back(v);
            return;
        }
        else {}
    }
    if (v.isNil()) m_dict.erase(k);
    else m_dict[k] = v;
}

void LuaTable::arrayInsert(int off, const LuaValue& v) {
    m_array.insert(m_array.begin() + off, v);
}

LuaValue LuaTable::arrayRemove(int off) {
    auto iter = m_array.begin() + off;
    auto r = *iter;
    m_array.erase(iter);
    return r;
}

LuaValue& LuaTable::getNext(LuaValue& k) {
    if (k.isNil()) {
        if (!m_array.empty()) {
            k = LuaValue(1);
            return m_array[0];
        }
        if (!m_dict.empty()) {
            k = m_dict.begin()->first;
            return m_dict.begin()->second;
        }
        return LuaValue::NIL;
    }

    if (k.isTypeOf(LVT_Number)) {
        int idx = (int)k.getNumber();
        if (idx >= 0 && idx < (int)m_array.size()) {
            k = LuaValue(idx + 1);
            return m_array[idx];
        } else if (idx == (int)m_array.size()) {
            if (!m_dict.empty()) {
                k = m_dict.begin()->first;
                return m_dict.begin()->second;
            }
        }
    }

    auto iter = m_dict.find(k);
    if (iter != m_dict.end()) {
        ++iter;
        if (iter != m_dict.end()) {
            k = iter->first;
            return iter->second;
        }
    }

    k = LuaValue::NIL;
    return LuaValue::NIL;
}
LuaValue& LuaTable::getINext(LuaValue& k) {
    if (k.isNil()) {
        if (!m_array.empty()) {
            k = LuaValue(1);
            return m_array[0];
        }
        return LuaValue::NIL;
    }

    int idx = (int)k.getNumber();
    if (idx >= 0 && idx < (int)m_array.size()) {
        k = LuaValue(idx + 1);
        return m_array[idx];
    }
    k = LuaValue::NIL;
    return LuaValue::NIL;
}

LuaValue LuaTable::getMeta(const char *metaName) {
    if (m_metaTable == NULL) return LuaValue::NIL;
    LuaValue v = m_metaTable->get(LuaValue(metaName));
    return v;
}

void LuaTable::sort() {
    std::sort(m_array.begin(), m_array.end());
}
void LuaTable::sort(const LuaValue& cmp) {
    vector<LuaValue> params, rets;
    std::sort(m_array.begin(), m_array.end(), [&params, &rets, &cmp]
            (const LuaValue& l, const LuaValue& r){
        params.clear(); rets.clear();
        params.push_back(l); params.push_back(r);
        callFunc(cmp, params, rets);
        return rets[0].getBoolean();
    });
}

LuaValue meta_add(LuaTable *table, const LuaValue& v) {
    return invokeMeta(table, table->m_metaTable, "__add", v);
}
LuaValue meta_sub(LuaTable *table, const LuaValue& v) {
    return invokeMeta(table, table->m_metaTable, "__sub", v);
}
LuaValue meta_mul(LuaTable *table, const LuaValue& v) {
    return invokeMeta(table, table->m_metaTable, "__mul", v);
}
LuaValue meta_div(LuaTable *table, const LuaValue& v) {
    return invokeMeta(table, table->m_metaTable, "__div", v);
}
LuaValue meta_mod(LuaTable *table, const LuaValue& v) {
    return invokeMeta(table, table->m_metaTable, "__mod", v);
}
LuaValue meta_pow(LuaTable *table, const LuaValue& v) {
    return invokeMeta(table, table->m_metaTable, "__pow", v);
}
LuaValue meta_concat(LuaTable *table, const LuaValue& v) {
    return invokeMeta(table, table->m_metaTable, "__concat", v);
}
LuaValue meta_eq(LuaTable *table, const LuaValue& v) {
    if (table->getMeta("__eq").isNil()) {
        return table == v.getTable() ? LuaValue::TRUE : LuaValue::FALSE;
    }
    return invokeMeta(table, table->m_metaTable, "__eq", v);
}
LuaValue meta_lt(LuaTable *table, const LuaValue& v) {
    return invokeMeta(table, table->m_metaTable, "__lt", v);
}
LuaValue meta_le(LuaTable *table, const LuaValue& v) {
    return invokeMeta(table, table->m_metaTable, "__le", v);
}
LuaValue meta_unm(LuaTable *table) {
    return invokeMeta(table, table->m_metaTable, "__unm", LuaValue::NIL);
}
void meta_call(LuaTable *table, LuaStackFrame* frame, int tableIdx, int paramCount, int requireRetN) {
    LuaValue m = table->m_metaTable->get(LuaValue("__call"));
    auto &values = frame->stack->values();
    values.insert(values.begin() + tableIdx, m);
    callFunc(tableIdx, paramCount + 1, requireRetN);
}

void LuaTable::collectGCObject(vector<GCObject*>& unscaned) {
    if (m_metaTable != NULL) {
        if (auto p = m_metaTable->gcAccess()) unscaned.push_back(p);
    }
    for (auto &v : m_array) {
        if (auto p = v.gcAccess()) unscaned.push_back(p);
    }
    for (auto &kv : m_dict) {
        if (auto p = kv.first.gcAccess()) unscaned.push_back(p);
        if (auto p = kv.second.gcAccess()) unscaned.push_back(p);
    }
}
