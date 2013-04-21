
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
    explicit LuaValue(NumberType num);
    explicit LuaValue(const char *str);
    LuaValue(const char *str, int size);
    explicit LuaValue(LuaTable* table);
    explicit LuaValue(Function* func);
    explicit LuaValue(LuaStack* stack);
    explicit LuaValue(LightUserData lud);
    ~LuaValue(){ m_type = LVT_Nil; }

    // LuaValue(const LuaValue& o);
    // LuaValue& operator = (const LuaValue& o);

    LuaValue& operator += (const LuaValue& o);
    LuaValue& operator -= (const LuaValue& o);
    LuaValue& operator *= (const LuaValue& o);
    LuaValue& operator /= (const LuaValue& o);
    LuaValue& operator %= (const LuaValue& o);

    bool operator == (const LuaValue& o) const;
    bool operator != (const LuaValue& o) const { return !(*this == o); }
    bool operator < (const LuaValue& o) const;
    bool operator <= (const LuaValue& o) const { return !(o < *this); }
    bool operator > (const LuaValue& o) const { return o < *this; }
    bool operator >= (const LuaValue& o) const { return !(*this < o); }

    LuaValueType getType() const { return m_type; }
    bool isTypeOf(LuaValueType t) const { return m_type == t; }
    bool isNil() const { return m_type == LVT_Nil; }
    bool getBoolean() const { 
        if (isNil()) return false;
        else if (isTypeOf(LVT_Boolean)) return m_data.b;
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

inline LuaValue operator + (const LuaValue& l, const LuaValue& r) {
    return LuaValue(l) += r;
}
inline LuaValue operator - (const LuaValue& l, const LuaValue& r) {
    return LuaValue(l) -= r;
}
inline LuaValue operator * (const LuaValue& l, const LuaValue& r) {
    return LuaValue(l) *= r;
}
inline LuaValue operator / (const LuaValue& l, const LuaValue& r) {
    return LuaValue(l) /= r;
}
inline LuaValue operator % (const LuaValue& l, const LuaValue& r) {
    return LuaValue(l) %= r;
}
LuaValue power(const LuaValue& l, const LuaValue& r);
LuaValue concat(const LuaValue& l, const LuaValue& r);

inline int LuaValue::getHash() const {
    switch (m_type) {
        case LVT_Nil: ASSERT(0);
        case LVT_Boolean: return (int)hash<bool>()(m_data.b);
        case LVT_Number: return (int)hash<NumberType>()(m_data.num);
        // TODO: check
        case LVT_String: return (int)hash<LuaString*>()(m_data.str);
        case LVT_Table: return (int)hash<LuaTable*>()(m_data.table);
        case LVT_Function: return (int)hash<Function*>()(m_data.func);
        case LVT_Stack: return (int)hash<LuaStack*>()(m_data.stack);
        case LVT_LightUserData: return (int)hash<LightUserData>()(m_data.lud);
        default: ASSERT(0);
    }
    return 0;
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
