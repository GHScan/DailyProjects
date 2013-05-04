
#ifndef LUA_VALUE_H
#define LUA_VALUE_H

typedef double NumberType;

class LuaString;
class LuaTable;
struct LuaStack;
struct Function;
struct GCObject;

struct LightUserDataRef {};
typedef LightUserDataRef* LightUserData;

#undef NIL
#undef TRUE
#undef FALSE

enum LuaValueType {
    LVT_Nil,
    LVT_Boolean,
    LVT_Number,
    LVT_String,
    LVT_Table,
    LVT_Function,
    LVT_Stack,
    LVT_LightUserData,
};
class LuaValue {
public:
    LuaValue(): m_type(LVT_Nil){}
    explicit LuaValue(NumberType num): m_type(LVT_Number) { m_data.num = num; }
    explicit LuaValue(const char *str);
    LuaValue(const char *str, int size);
    explicit LuaValue(LuaTable* table): m_type(LVT_Table){ m_data.table = table; }
    explicit LuaValue(Function* func): m_type(LVT_Function) { m_data.func = func; }
    explicit LuaValue(LuaStack* stack): m_type(LVT_Stack) { m_data.stack = stack; }
    explicit LuaValue(LightUserData lud): m_type(LVT_LightUserData) { m_data.lud = lud; }
    ~LuaValue(){ m_type = LVT_Nil; }

    // LuaValue(const LuaValue& o);
    // LuaValue& operator = (const LuaValue& o);

    void addFrom(const LuaValue& l, const LuaValue& r);
    void subFrom(const LuaValue& l, const LuaValue& r);
    void mulFrom(const LuaValue& l, const LuaValue& r);
    void divFrom(const LuaValue& l, const LuaValue& r);
    void modFrom(const LuaValue& l, const LuaValue& r);
    void powFrom(const LuaValue& l, const LuaValue& r);
    void concatFrom(const LuaValue& l, const LuaValue& r);

    void notFrom(const LuaValue& l);
    void lenFrom(const LuaValue& l);
    void minusFrom(const LuaValue& l);

    void lessFrom(const LuaValue& l, const LuaValue& r);
    void lessEqFrom(const LuaValue& l, const LuaValue& r);
    void greaterFrom(const LuaValue& l, const LuaValue& r);
    void greaterEqFrom(const LuaValue& l, const LuaValue& r);
    void equalFrom(const LuaValue& l, const LuaValue& r);
    void nequalFrom(const LuaValue& l, const LuaValue& r);

    bool operator == (const LuaValue& o) const;
    bool operator != (const LuaValue& o) const;
    bool operator < (const LuaValue& o) const;
    bool operator <= (const LuaValue& o) const;
    bool operator > (const LuaValue& o) const;
    bool operator >= (const LuaValue& o) const;
    LuaValue operator + (const LuaValue& o) const;
    LuaValue operator - (const LuaValue& o) const;
    LuaValue operator * (const LuaValue& o) const;
    LuaValue operator / (const LuaValue& o) const;
    LuaValue operator % (const LuaValue& o) const;

    LuaValueType getType() const { return m_type; }
    bool isTypeOf(LuaValueType t) const { return m_type == t; }
    bool isNil() const { return m_type == LVT_Nil; }
    bool getBoolean() const { 
        if (isTypeOf(LVT_Boolean)) return m_data.b;
        else if (isNil()) return false;
        else return true;
    }
    NumberType getNumber() const { ASSERT(isTypeOf(LVT_Number)); return m_data.num; }
    LuaString* getString() const { ASSERT(isTypeOf(LVT_String)); return m_data.str;}
    LuaTable* getTable() const { ASSERT(isTypeOf(LVT_Table)); return m_data.table; }
    Function* getFunction() const { ASSERT(isTypeOf(LVT_Function)); return m_data.func;}
    LuaStack* getStack() const { ASSERT(isTypeOf(LVT_Stack)); return m_data.stack;}
    LightUserData getLightUserData() const { ASSERT(isTypeOf(LVT_LightUserData)); return m_data.lud; }

    GCObject* gcAccess() const;

    int getHash() const;
    int getSize() const;
    string toString() const;
public:
    static LuaValue NIL;
    static LuaValue TRUE;
    static LuaValue FALSE;

private:
    static LuaValue fromBoolean(bool b) {
        LuaValue r; r.m_type = LVT_Boolean; r.m_data.b = b;
        return r;
    }

    void _lessFrom(const LuaValue& l, const LuaValue& r);
    void _lessEqFrom(const LuaValue& l, const LuaValue& r);
private:
    LuaValueType m_type;
    union {
        bool b;
        NumberType num;
        LuaString* str;
        LuaTable* table;
        Function* func;
        LuaStack* stack;
        LightUserData lud;
    } m_data;
};

extern LuaValue meta_add(LuaTable *table, const LuaValue& v);
extern LuaValue meta_sub(LuaTable *table, const LuaValue& v);
extern LuaValue meta_mul(LuaTable *table, const LuaValue& v);
extern LuaValue meta_div(LuaTable *table, const LuaValue& v);
extern LuaValue meta_mod(LuaTable *table, const LuaValue& v);
extern LuaValue meta_pow(LuaTable *table, const LuaValue& v);
extern LuaValue meta_concat(LuaTable *table, const LuaValue& v);
extern LuaValue meta_eq(LuaTable *table, const LuaValue& v);
extern LuaValue meta_lt(LuaTable *table, const LuaValue& v);
extern LuaValue meta_le(LuaTable *table, const LuaValue& v);
extern LuaValue meta_unm(LuaTable *table);

FORCE_INLINE void LuaValue::addFrom(const LuaValue& l, const LuaValue& r) {
    if (l.isTypeOf(LVT_Number)) {
        ASSERT(r.isTypeOf(LVT_Number));
        m_type = LVT_Number;
        m_data.num = l.m_data.num + r.m_data.num;
    } else {
        *this = meta_add(l.getTable(), r);
    }
}
FORCE_INLINE void LuaValue::subFrom(const LuaValue& l, const LuaValue& r) {
    if (l.isTypeOf(LVT_Number)) {
        ASSERT(r.isTypeOf(LVT_Number));
        m_type = LVT_Number;
        m_data.num = l.m_data.num - r.m_data.num;
    } else {
        *this = meta_sub(l.getTable(), r);
    }
}
FORCE_INLINE void LuaValue::mulFrom(const LuaValue& l, const LuaValue& r) {
    if (l.isTypeOf(LVT_Number)) {
        ASSERT(r.isTypeOf(LVT_Number));
        m_type = LVT_Number;
        m_data.num = l.m_data.num * r.m_data.num;
    } else {
        *this = meta_mul(l.getTable(), r);
    }
}
FORCE_INLINE void LuaValue::divFrom(const LuaValue& l, const LuaValue& r) {
    if (l.isTypeOf(LVT_Number)) {
        ASSERT(r.isTypeOf(LVT_Number));
        m_type = LVT_Number;
        m_data.num = l.m_data.num / r.m_data.num;
    } else {
        *this = meta_div(l.getTable(), r);
    }
}
FORCE_INLINE void LuaValue::modFrom(const LuaValue& l, const LuaValue& r) {
    if (l.isTypeOf(LVT_Number)) {
        ASSERT(r.isTypeOf(LVT_Number));
        m_type = LVT_Number;
        m_data.num = ::fmod(l.m_data.num, r.m_data.num);
    } else {
        *this = meta_mod(l.getTable(), r);
    }
}
FORCE_INLINE void LuaValue::powFrom(const LuaValue& l, const LuaValue& r) {
    if (l.isTypeOf(LVT_Number)) {
        ASSERT(r.isTypeOf(LVT_Number));
        m_type = LVT_Number;
        m_data.num = ::pow(l.m_data.num, r.m_data.num);
    } else {
        *this = meta_pow(l.getTable(), r);
    }
}

FORCE_INLINE void LuaValue::notFrom(const LuaValue& l) {
    m_type = LVT_Boolean;
    m_data.b = !l.getBoolean();
}
FORCE_INLINE void LuaValue::lenFrom(const LuaValue& l) {
    m_type = LVT_Number;
    m_data.num = l.getSize();
}
FORCE_INLINE void LuaValue::minusFrom(const LuaValue& l) {
    if (l.isTypeOf(LVT_Number)) {
        m_type = LVT_Number;
        m_data.num = -l.m_data.num;
    } else {
        *this = meta_unm(l.getTable());
    }
}

FORCE_INLINE void LuaValue::lessFrom(const LuaValue& l, const LuaValue& r) {
    if (l.isTypeOf(LVT_Number)) {
        ASSERT(r.isTypeOf(LVT_Number));
        m_type = LVT_Boolean;
        m_data.b = l.m_data.num < r.m_data.num;
    } else {
        _lessFrom(l, r);
    }
}
FORCE_INLINE void LuaValue::lessEqFrom(const LuaValue& l, const LuaValue& r) {
    if (l.isTypeOf(LVT_Number)) {
        ASSERT(r.isTypeOf(LVT_Number));
        m_type = LVT_Boolean;
        m_data.b = l.m_data.num <= r.m_data.num;
    } else {
        _lessEqFrom(l, r);
    }
}
FORCE_INLINE void LuaValue::greaterFrom(const LuaValue& l, const LuaValue& r) {
    lessFrom(r, l);
}
FORCE_INLINE void LuaValue::greaterEqFrom(const LuaValue& l, const LuaValue& r) {
    lessEqFrom(r, l);
}
FORCE_INLINE void LuaValue::equalFrom(const LuaValue& l, const LuaValue& r) {
    m_type = LVT_Boolean;
    if (l.m_type == r.m_type) {
        switch (l.m_type) {
        case LVT_Nil: m_data.b = true; break;
        case LVT_Boolean: m_data.b = l.m_data.b == r.m_data.b; break;
        case LVT_Number: m_data.b = l.m_data.num == r.m_data.num; break;
        case LVT_String: m_data.b = l.m_data.str == r.m_data.str; break;
        case LVT_Table: *this = meta_eq(l.m_data.table, r); break;
        case LVT_Function: m_data.b = l.m_data.func == r.m_data.func; break;
        case LVT_Stack: m_data.b = l.m_data.stack == r.m_data.stack; break;
        case LVT_LightUserData: m_data.b = l.m_data.lud == r.m_data.lud; break;
        default: ASSERT(0);
        }
    } else {
        m_data.b = false;
    }
}
FORCE_INLINE void LuaValue::nequalFrom(const LuaValue& l, const LuaValue& r) {
    equalFrom(l, r);
    m_data.b = !m_data.b;
}

inline int LuaValue::getHash() const {
    switch (m_type) {
        case LVT_Nil: ASSERT(0);
        case LVT_Boolean: return (int)hash<bool>()(m_data.b);
        case LVT_Number: return (int)hash<NumberType>()(m_data.num);
        case LVT_String: return (int)hash<LuaString*>()(m_data.str);
        case LVT_Table: return (int)hash<LuaTable*>()(m_data.table);
        case LVT_Function: return (int)hash<Function*>()(m_data.func);
        case LVT_Stack: return (int)hash<LuaStack*>()(m_data.stack);
        case LVT_LightUserData: return (int)hash<LightUserData>()(m_data.lud);
        default: ASSERT(0);
    }
    return 0;
}

inline bool LuaValue::operator == (const LuaValue& o) const {
    LuaValue v; v.equalFrom(*this, o);
    return v.getBoolean();
}
inline bool LuaValue::operator != (const LuaValue& o) const {
    LuaValue v; v.nequalFrom(*this, o);
    return v.getBoolean();
}
inline bool LuaValue::operator < (const LuaValue& o) const {
    LuaValue v; v.lessFrom(*this, o);
    return v.getBoolean();
}
inline bool LuaValue::operator <= (const LuaValue& o) const {
    LuaValue v; v.lessEqFrom(*this, o);
    return v.getBoolean();
}
inline bool LuaValue::operator > (const LuaValue& o) const {
    LuaValue v; v.greaterFrom(*this, o);
    return v.getBoolean();
}
inline bool LuaValue::operator >= (const LuaValue& o) const {
    LuaValue v; v.greaterEqFrom(*this, o);
    return v.getBoolean();
}
inline LuaValue LuaValue::operator + (const LuaValue& o) const {
    LuaValue v; v.addFrom(*this, o);
    return v;
}
inline LuaValue LuaValue::operator - (const LuaValue& o) const {
    LuaValue v; v.subFrom(*this, o);
    return v;
}
inline LuaValue LuaValue::operator * (const LuaValue& o) const {
    LuaValue v; v.mulFrom(*this, o);
    return v;
}
inline LuaValue LuaValue::operator / (const LuaValue& o) const {
    LuaValue v; v.divFrom(*this, o);
    return v;
}
inline LuaValue LuaValue::operator % (const LuaValue& o) const {
    LuaValue v; v.modFrom(*this, o);
    return v;
}

namespace std{

template<>
struct hash<LuaValue> {
    int operator () (const LuaValue& v) const {
        return v.getHash();
    }
};

}

#endif
