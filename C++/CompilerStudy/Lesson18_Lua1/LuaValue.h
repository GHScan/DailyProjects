
#ifndef VALUE_H
#define VALUE_H

typedef double NumberType;

enum LuaValueType
{
    LVT_Nil,
    LVT_Boolean,
    LVT_Number,
    LVT_String,
    LVT_Table,
};

class LuaValue;
class LuaTable;

template <class T>
inline void hash_combine(int & seed, const T & v)
{
    std::hash<T> hasher;
    seed ^= hasher(v) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
}

//======== LuaValue ============
class LuaValue
{
public:
    explicit LuaValue(NumberType n): m_type(LVT_Number){ m_data.n = n; }
    explicit LuaValue(const char *str);
    LuaValue(LuaValueType t, int v = 0);

    LuaValue(): m_type(LVT_Nil) {m_data.n = 0;}
    LuaValue(const LuaValue& o);
    LuaValue(LuaValue&& o);
    LuaValue& operator = (const LuaValue& o);
    LuaValue& operator = (LuaValue&& o);
    ~LuaValue();

    bool isTypeOf(LuaValueType t) const { return m_type == t; }
    bool getBoolean() const { ASSERT(isTypeOf(LVT_Boolean)); return m_data.b;}
    NumberType getNumber() const { ASSERT(isTypeOf(LVT_Number)); return m_data.n; }
    const char *getString() const { ASSERT(isTypeOf(LVT_String)); return m_data.str;}
    const LuaTable* getLuaTable() const { ASSERT(isTypeOf(LVT_Table)); return m_data.table;}
    LuaTable* getLuaTable() { ASSERT(isTypeOf(LVT_Table)); return m_data.table;}

    bool operator == (const LuaValue& o) const;
    bool operator != (const LuaValue& o) const { return !(*this == o);}
    bool operator < (const LuaValue& o) const;
    bool operator <= (const LuaValue& o) const { return *this == o || *this < o;}
    bool operator > (const LuaValue& o) const { return !(*this <= o);}
    bool operator >= (const LuaValue& o) const { return !(*this < o);}

    LuaValue& operator += (const LuaValue& o);
    LuaValue& operator -= (const LuaValue& o);
    LuaValue& operator *= (const LuaValue& o);
    LuaValue& operator /= (const LuaValue& o);
    LuaValue& operator %= (const LuaValue& o);

    const LuaValue& get(const LuaValue& k);
    void set(const LuaValue& k, const LuaValue& v);

    int getHash() const;

    string toString() const;
public:
    static LuaValue NIL;
    static LuaValue TRUE;
    static LuaValue FALSE;

private:
    LuaValueType m_type;
    union {
        bool b;
        NumberType n;
        char *str;
        LuaTable *table;
    } m_data;
};

namespace std
{
    template<>
    struct hash<LuaValue>
    {
        int operator () (const LuaValue& v) const
        {
            return v.getHash();
        }
    };
}

//======== LuaTable ============

class LuaTable
{
public:
    static LuaTable* create()
    {
        return new LuaTable();
    }

    const LuaValue& get(const LuaValue& k) const;
    void set(const LuaValue& k, const LuaValue& v);

    int size() const { return (int)m_vec.size();}

    const LuaValue& getNext(LuaValue& k) const;

    bool operator == (const LuaTable& o) const
    {
        return m_vec == o.m_vec && m_hashTable == o.m_hashTable;
    }
    bool operator != (const LuaTable& o) const { return !(*this == o); }

    int getHash() const;

    int getRefCount() const { return m_refCount;}
    int addRef() { return ++m_refCount;}
    int releaseRef() 
    {
        int r = --m_refCount;
        if (r == 0) delete this;
        return r;
    }

private:
    LuaTable(): m_hash(0), m_refCount(1){}
    LuaTable(const LuaTable&);
    LuaTable& operator = (const LuaTable&);

private:
    vector<LuaValue> m_vec;
    unordered_map<LuaValue, LuaValue> m_hashTable;
    mutable int m_hash;
    int m_refCount;
};

#endif
