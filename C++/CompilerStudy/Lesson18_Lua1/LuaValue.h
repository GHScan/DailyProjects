
#ifndef LUA_VALUE_H
#define LUA_VALUE_H

typedef double NumberType;

enum LuaValueType {
    LVT_Nil,
    LVT_Boolean,
    LVT_Number,
    LVT_String,
    LVT_Table,
    LVT_Function,
    LVT_LightUserData,
    // This is used to mask the special location as reference, it will only
    // exist on local/upvalue variable. It will be better to use a standard
    // flag to be this property instead of use LuaValueType, but we can get
    // the benifit of memory usage in this way, so...
    // We should prevent something like 'locals.resize(newsize)' to be
    // happened, becase it will cause the property of the local lost. Shared
    // flag can't be move to another memory address automaticly.
    LVT_SharedLuaValue, 
};

class LuaTable;
class SharedLuaValue;
struct IFunction;

struct LightUserDataRef{};
typedef LightUserDataRef* LightUserData;

class LuaValue
{
public:
    LuaValue(): m_type(LVT_Nil) {m_data.n = 0;}
    explicit LuaValue(NumberType n): m_type(LVT_Number){ m_data.n = n; }
    explicit LuaValue(const char* str);
    explicit LuaValue(IFunction *func);
    explicit LuaValue(LuaTable *table);
    explicit LuaValue(LightUserData lud);

    LuaValue(const LuaValue& o);
    LuaValue(LuaValue&& o);
    LuaValue& operator = (const LuaValue& o);
    LuaValue& operator = (LuaValue&& o);
    ~LuaValue();

    LuaValueType getType() const;
    bool isTypeOf(LuaValueType t) const { return getType() == t; }

    bool getBoolean() const;
    NumberType getNumber() const ;
    const char *getString() const;
    LuaTable* getTable() const;
    IFunction* getFunction() const ;
    LightUserData getLightUserData() const ;

    bool operator == (const LuaValue& o) const;
    bool operator != (const LuaValue& o) const { return !(*this == o);}
    bool operator < (const LuaValue& o) const;
    bool operator <= (const LuaValue& o) const;
    bool operator > (const LuaValue& o) const { return !(*this <= o);}
    bool operator >= (const LuaValue& o) const { return !(*this < o);}

    LuaValue& operator += (const LuaValue& o);
    LuaValue& operator -= (const LuaValue& o);
    LuaValue& operator *= (const LuaValue& o);
    LuaValue& operator /= (const LuaValue& o);
    LuaValue& operator %= (const LuaValue& o);

    LuaValue power(const LuaValue& o) const;
    LuaValue concat(const LuaValue& o) const;

    int getSize() const;

    int getHash() const;

    string toString() const;

    void shareWith(LuaValue& o);
    void disableShared();

public:
    static LuaValue NIL;
    static LuaValue TRUE;
    static LuaValue FALSE;

private:
    static LuaValue createBoolean(bool b) {
        LuaValue r; r.m_type = LVT_Boolean; r.m_data.b = b;
        return r;
    }

private:
    LuaValueType m_type;
    union {
        bool b;
        NumberType n;
        char *str;
        LuaTable *table;
        IFunction *func;
        LightUserData lud;
        SharedLuaValue *shared;
    } m_data;
};
LuaValue operator + (const LuaValue& a, const LuaValue &b);
LuaValue operator - (const LuaValue& a, const LuaValue &b);
LuaValue operator * (const LuaValue& a, const LuaValue &b);
LuaValue operator / (const LuaValue& a, const LuaValue &b);
LuaValue operator % (const LuaValue& a, const LuaValue &b);

class SharedLuaValue {
public:
    static SharedLuaValue* create(const LuaValue& v) {
        return new SharedLuaValue(v);
    }

    LuaValue& value() { return m_value;}

    int getRefCount() const { return m_refCount;}
    int addRef() const { return ++m_refCount;}
    int releaseRef() const {
        int r = --m_refCount;
        if (r == 0) delete this;
        return r;
    }
private:
    SharedLuaValue(const SharedLuaValue&);
    SharedLuaValue& operator = (const SharedLuaValue&);
    SharedLuaValue(const LuaValue& v): m_value(v), m_refCount(1){}
    ~SharedLuaValue(){}

private:
    LuaValue m_value;
    mutable int m_refCount;
};

inline LuaValueType LuaValue::getType() const { 
    if (m_type == LVT_SharedLuaValue) return m_data.shared->value().getType();
    return m_type; 
}

inline bool LuaValue::getBoolean() const { 
    switch (m_type) {
        case LVT_SharedLuaValue: return m_data.shared->value().getBoolean();
        case LVT_Nil: return false;
        case LVT_Boolean: return m_data.b;
        default: return true;
    }
}
inline NumberType LuaValue::getNumber() const { 
    if (m_type == LVT_SharedLuaValue) return m_data.shared->value().getNumber();
    ASSERT(m_type == LVT_Number);
    return m_data.n;
}
inline const char * LuaValue::getString() const { 
    if (m_type == LVT_SharedLuaValue) return m_data.shared->value().getString();
    ASSERT(m_type == LVT_String); 
    return m_data.str;
}
inline LuaTable* LuaValue::getTable() const { 
    if (m_type == LVT_SharedLuaValue) return m_data.shared->value().getTable();
    ASSERT(m_type == LVT_Table); 
    return m_data.table; 
}
inline IFunction* LuaValue::getFunction() const { 
    if (m_type == LVT_SharedLuaValue) return m_data.shared->value().getFunction();
    ASSERT(m_type == LVT_Function);
    return m_data.func;
}
inline LightUserData LuaValue::getLightUserData() const { 
    if (m_type == LVT_SharedLuaValue) return m_data.shared->value().getLightUserData();
    ASSERT(m_type == LVT_LightUserData); 
    return m_data.lud; 
}

namespace std {
    template<>
    struct hash<LuaValue> {
        int operator () (const LuaValue& v) const {
            return v.getHash();
        }
    };
}

#endif
