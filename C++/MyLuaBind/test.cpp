// vim:fileencoding=gbk

#include "pch.h"

#include <cmath>
#include <cassert>

#include <string>
#include <map>
#include <vector>

#include <lua.hpp>

#pragma warning(disable : 4996)

lua_State* getLuaState();

class LuaTable;

class LuaStackBalanceKeeper
{
public:
    LuaStackBalanceKeeper(lua_State *l): m_l(l) 
    {
        m_top = lua_gettop(m_l); 
    }
    ~LuaStackBalanceKeeper() 
    { 
        lua_settop(m_l, m_top); 
    }
private:
    int m_top;
    lua_State *m_l;
};

void dumpStack(lua_State *l)
{
    char buf[128] = "";

    int n = lua_gettop(l);
    for (int i = 1; i <= n; ++i) {
        int type = lua_type(l, i);
        switch (type) {
            case LUA_TNIL:
                printf("%s,", "nil");
                break;
            case LUA_TNUMBER:
                printf("%f,", lua_tonumber(l, i));
                break;
            case LUA_TSTRING:
                printf("%s,", lua_tostring(l, i));
                break;
            case LUA_TBOOLEAN:
                printf("%s,", lua_toboolean(l, i) ? "true" : "false");
                break;
            case LUA_TTABLE:
            case LUA_TLIGHTUSERDATA:
            case LUA_TUSERDATA:
            case LUA_TFUNCTION:
            case LUA_TTHREAD:
                printf("%s,", lua_typename(l, type));
                break;
            default:
                assert(0);
                break;
        }
    }
    puts("");
}

inline bool fequal(double l, double r)
{
    return fabs(l - r) < 0.00001;
}

template<bool>
class Copyable {};
template<>
class Copyable<false>
{
public:
    Copyable(){}
private:
    Copyable(const Copyable&);
    Copyable& operator = (const Copyable&);
};

template<typename T> struct RemoveConst { typedef T Type; };
template<typename T> struct RemoveConst<const T> { typedef T Type; };
template<typename T> struct RemoveVolatile { typedef T Type; };
template<typename T> struct RemoveVolatile<volatile T> { typedef T Type; };
template<typename T> struct RemoveCV { typedef typename RemoveVolatile<typename RemoveConst<T>::Type>::Type Type; };
template<typename T> struct RemovePointer { typedef T Type; };
template<typename T> struct RemovePointer<T*> { typedef T Type; };
template<typename T> struct RemoveReference { typedef T Type; };
template<typename T> struct RemoveReference<T&> { typedef T Type; };
template<typename T>
struct RawType
{
    typedef typename RemoveCV<
        typename RemoveReference<
        typename RemovePointer<
        typename RemoveCV<T>::Type>::Type>::Type>::Type 
        Type;
};
template<typename T>
struct VariableType
{
    typedef typename RemoveConst<
        typename RemoveReference<T>::Type>::Type
        Type;
};

struct Interface
{
    virtual ~Interface() = 0 {}
};

class CppTypeInfo:
    public Copyable<true>
{
public:
    /* explicit */ CppTypeInfo(const std::type_info& info): m_info(info){}
    bool operator == (const CppTypeInfo& o) const { return m_info == o.m_info; }
    bool operator < (const CppTypeInfo& o) const { return m_info.before(o.m_info) < 0; }
private:
    const std::type_info &m_info;
};

class TypeNameManager:
    public Copyable<false>
{
public:
    void registerType(CppTypeInfo info, const std::string& name)
    {
        assert(m_map.count(info) == 0);
        m_map[info] = name;
    }
    const std::string& getTypeName(CppTypeInfo info) const 
    { 
        std::map<CppTypeInfo, std::string>::const_iterator iter = m_map.find(info);
        assert(iter != m_map.end());
        return iter->second;
    }
private:
    std::map<CppTypeInfo, std::string> m_map;
};

class UserdataNameManager:
    public TypeNameManager
{
public:
    static UserdataNameManager* instance() 
    {
        static UserdataNameManager s_ins;
        return &s_ins;
    }
private:
    UserdataNameManager(){}
    ~UserdataNameManager(){}
};

struct Type_Integer{};
struct Type_Float{};
struct Type_String{};
struct Type_Table{};
struct Type_Userdefined{};
template<typename T>
struct TypeTraits
{
    typedef Type_Userdefined Type;
};
template<> struct TypeTraits<char> { typedef Type_Integer Type; };
template<> struct TypeTraits<unsigned char> { typedef Type_Integer Type; };
template<> struct TypeTraits<short> { typedef Type_Integer Type; };
template<> struct TypeTraits<unsigned short> { typedef Type_Integer Type; };
template<> struct TypeTraits<int> { typedef Type_Integer Type; };
template<> struct TypeTraits<unsigned int> { typedef Type_Integer Type; };
template<> struct TypeTraits<long> { typedef Type_Integer Type; };
template<> struct TypeTraits<unsigned long> { typedef Type_Integer Type; };
template<> struct TypeTraits<float> { typedef Type_Float Type; };
template<> struct TypeTraits<double> { typedef Type_Float Type; };
template<> struct TypeTraits<char*> { typedef Type_String Type; };
template<> struct TypeTraits<const char*> { typedef Type_String Type; };
template<> struct TypeTraits<std::string> { typedef Type_String Type; };
template<> struct TypeTraits<LuaTable> { typedef Type_Table Type; };

template<typename T>
void _luaStack_get(lua_State* l, int idx, T& val, Type_Integer)
{
    lua_Integer i = luaL_checkinteger(l, idx);
    val = static_cast<T>(i);
    assert(val == i);
}
template<typename T>
void _luaStack_get(lua_State* l, int idx, T& val, Type_Float)
{
    lua_Number d = luaL_checknumber(l, idx);
    val = static_cast<T>(d);
    assert(fequal(d, val));
}
template<typename T>
void _luaStack_get(lua_State* l, int idx, T& val, Type_String)
{
    const char* s = luaL_checkstring(l, idx);
    val = static_cast<T>(s);
    assert(val == s);
}
template<typename T>
void _luaStack_get(lua_State* l, int idx, T& val, Type_Table)
{
    luaL_checktype(l, idx, LUA_TTABLE);
    val = LuaTable(l, idx);
}
// T is a pointer, for example, A*
template<typename T>
void _luaStack_get(lua_State* l, int idx, T& val, Type_Userdefined)
{
    void *data = luaL_checkudata(l, idx, UserdataNameManager::instance()->getTypeName(
                typeid(typename RawType<T>::Type)).c_str());
    val = static_cast<T>(data);
}

template<typename T>
void luaStack_get(lua_State *l, int idx, T& val)
{
    _luaStack_get(l, idx, val, typename TypeTraits<T>::Type());
}

template<typename T>
void _luaStack_set(lua_State* l, int idx, const T& val, Type_Integer)
{
    lua_Integer i = static_cast<lua_Integer>(val);
    assert(i == val);
    lua_pushinteger(l, i);
    lua_replace(l, idx);
}
template<typename T>
void _luaStack_set(lua_State* l, int idx, const T& val, Type_Float)
{
    lua_Number d = static_cast<lua_Number>(val);
    assert(fequal(d, val));
    lua_pushnubmer(l, d);
    lua_replace(l, idx);
}
template<typename T>
void _luaStack_set(lua_State* l, int idx, const T& val, Type_String)
{
    lua_pushstring(std::string(val).c_str());
    lua_replace(l, idx);
}
template<typename T>
void _luaStack_set(lua_State* l, int idx, const T& val, Type_Table)
{
    val.push2Stack();
    lua_replace(l, idx);
}

template<typename T>
void luaStack_set(lua_State* l, int idx, const T& val)
{
    _luaStack_set(l, idx, val, typename TypeTraits<T>::Type());
}

template<typename T>
void _luaStack_push(lua_State* l, const T& val, Type_Integer)
{
    lua_Integer i = static_cast<lua_Integer>(val);
    assert(i == val);
    lua_pushinteger(l, i);
}
template<typename T>
void _luaStack_push(lua_State* l, const T& val, Type_Float)
{
    lua_Number d = static_cast<lua_Number>(val);
    assert(fequal(d, val));
    lua_pushnumber(l, d);
}
template<typename T>
void _luaStack_push(lua_State* l, const T& val, Type_String)
{
    lua_pushstring(l, std::string(val).c_str());
}
template<typename T>
void _luaStack_push(lua_State* l, const T& val, Type_Table)
{
    val.push2Stack();
}

template<typename T>
void luaStack_push(lua_State* l, const T& val)
{
    _luaStack_push(l, val, typename TypeTraits<T>::Type());
}

template<typename T>
void luaCall0_1(lua_State *l, const char *fname, T val)
{
    LuaStackBalanceKeeper stackKeeper(l);
    lua_getglobal(l, fname);
    luaStack_push(l, val);
    lua_call(l, 1, 0);
}
template<typename R, typename T>
R luaCall1_1(lua_State *l, const char *fname, T val)
{
    LuaStackBalanceKeeper stackKeeper(l);
    lua_getglobal(l, fname);
    luaStack_push(l, val);
    lua_call(l, 1, 1);
    typename VariableType<R>::Type r;
    luaStack_get(l, -1, r);
    return r;
}

template<typename T>
struct LuaCallbackWrapper0_1
{
    static int l_call(lua_State* l)
    {
        typedef void (*FuncT) (T);
        FuncT f = static_cast<FuncT>(lua_touserdata(l, lua_upvalueindex(1)));
        typename VariableType<T>::Type val;
        luaStack_get(l, -1, val);
        f(val);
        return 0;
    }
};

template<typename R, typename T>
struct LuaCallbackWrapper1_1
{
    static int l_call(lua_State* l)
    {
        typedef R (*FuncT) (T);
        FuncT f = static_cast<FuncT>(lua_touserdata(l, lua_upvalueindex(1)));
        typename VariableType<T>::Type val;
        luaStack_get(l, -1, val);
        typename VariableType<R>::Type r = f(val);
        luaStack_push(l, r);
        return 1;
    }
};

template<typename R>
struct LuaCallbackWrapper1_0
{
    static int l_call(lua_State* l)
    {
        typedef R (*FuncT) ();
        FuncT f = static_cast<FuncT>(lua_touserdata(l, lua_upvalueindex(1)));
        typename VariableType<R>::Type r = f();
        luaStack_push(l, r);
        return 1;
    }
};

template<typename T>
void luaRegisterFunction0_1(lua_State *l, const char *fname, void(*f)(T))
{
    lua_pushlightuserdata(l, f);
    lua_pushcclosure(l, &LuaCallbackWrapper0_1<T>::l_call, 1);
    lua_setglobal(l, fname);
}

template<typename R, typename T>
void luaRegisterFunction1_1(lua_State *l, const char *fname, R(*f)(T))
{
    lua_pushlightuserdata(l, f);
    lua_pushcclosure(l, &LuaCallbackWrapper1_1<R, T>::l_call, 1);
    lua_setglobal(l, fname);
}

template<typename R>
void luaRegisterFunction1_0(lua_State *l, const char *fname, R(*f)())
{
    lua_pushlightuserdata(l, f);
    lua_pushcclosure(l, &LuaCallbackWrapper1_0<R>::l_call, 1);
    lua_setglobal(l, fname);
}

template<typename T>
struct LuaMethodCallbackWrapper0_1
{
    static int l_call(lua_State *l)
    {
        typedef void(LuaMethodCallbackWrapper0_1::*FuncT)(T);
        FuncT f = force_cast<FuncT>(lua_touserdata(l, lua_upvalueindex(1)));
        const char *className = lua_tostring(l, lua_upvalueindex(2));

        LuaMethodCallbackWrapper0_1* _this = static_cast<LuaMethodCallbackWrapper0_1*>(
                luaL_checkudata(l, 1, className));
        typename VariableType<T>::Type val;
        luaStack_get(l, 2, val);
        (_this->*f)(val);
        return 0;
    }
};

template<typename T, typename T2>
struct LuaMethodCallbackWrapper0_2
{
    static int l_call(lua_State *l)
    {
        typedef void(LuaMethodCallbackWrapper0_2::*FuncT)(T, T2);
        FuncT f = force_cast<FuncT>(lua_touserdata(l, lua_upvalueindex(1)));
        const char *className = lua_tostring(l, lua_upvalueindex(2));

        LuaMethodCallbackWrapper0_2* _this = static_cast<LuaMethodCallbackWrapper0_2*>(
                luaL_checkudata(l, 1, className));
        typename VariableType<T>::Type val;
        typename VariableType<T2>::Type val2;
        luaStack_get(l, 2, val);
        luaStack_get(l, 3, val2);
        (_this->*f)(val, val2);
        return 0;
    }
};

template<typename R, typename T>
struct LuaMethodCallbackWrapper1_1
{
    static int l_call(lua_State *l)
    {
        typedef R (LuaMethodCallbackWrapper1_1::*FuncT)(T);
        FuncT f = force_cast<FuncT>(lua_touserdata(l, lua_upvalueindex(1)));
        const char *className = lua_tostring(l, lua_upvalueindex(2));

        LuaMethodCallbackWrapper1_1* _this = static_cast<LuaMethodCallbackWrapper1_1*>(
                luaL_checkudata(l, 1, className));
        typename VariableType<T>::Type val;
        luaStack_get(l, 2, val);
        typename VariableType<R>::Type r = (_this->*f)(val);
        luaStack_push(l, r);
        return 1;
    }
};

template<typename R>
struct LuaMethodCallbackWrapper1_0
{
    static int l_call(lua_State *l)
    {
        typedef R (LuaMethodCallbackWrapper1_0::*FuncT)();
        FuncT f = force_cast<FuncT>(lua_touserdata(l, lua_upvalueindex(1)));
        const char *className = lua_tostring(l, lua_upvalueindex(2));

        LuaMethodCallbackWrapper1_0* _this = static_cast<LuaMethodCallbackWrapper1_0*>(
                luaL_checkudata(l, 1, className));
        typename VariableType<R>::Type r = (_this->*f)();
        luaStack_push(l, r);
        return 1;
    }
};

template<typename DestT, typename SrcT>
DestT force_cast(SrcT s)
{
    union {
        SrcT src;
        DestT dest;
    } o = {s};
    return o.dest;
}

template<typename ClassT>
class luaClassRegister
{
public:
    luaClassRegister(lua_State *l, const char *className):
        m_l(l), m_className(className)
    {
        // class table
        lua_newtable(l);
        lua_pushvalue(l, -1);
        lua_setglobal(l, className);
        // metatable 
        luaL_newmetatable(l, className);
        lua_pushvalue(l, -1);
        lua_setmetatable(l, -3);

        lua_pushvalue(l, -1);
        lua_setfield(l, -2, "__index");
        // gc
        lua_pushstring(l, className);
        lua_pushcclosure(l, &l_destruct, 1);
        lua_setfield(l, -2, "__gc");

        UserdataNameManager::instance()->registerType(typeid(ClassT), className);
    }

    template<typename T>
    luaClassRegister& constructor()
    {
        lua_pushstring(m_l, m_className.c_str());
        lua_pushcclosure(m_l, &l_construct<T>, 1);
        lua_setfield(m_l, -3, "new");
        return *this;
    }

    template<typename T>
    luaClassRegister& method0_1(const char *fname, void(ClassT::*f)(T))
    {
        return _method(fname, 
                force_cast<lua_CFunction>(&LuaMethodCallbackWrapper0_1<T>::l_call),
                force_cast<void*>(f));
    }
    template<typename T, typename T2>
    luaClassRegister& method0_2(const char *fname, void(ClassT::*f)(T, T2))
    {
        return _method(fname, 
                force_cast<lua_CFunction>(&LuaMethodCallbackWrapper0_2<T, T2>::l_call),
                force_cast<void*>(f));
    }

    template<typename R, typename T>
    luaClassRegister& method1_1(const char *fname, R(ClassT::*f)(T))
    {
        return _method(fname, 
                force_cast<lua_CFunction>(&LuaMethodCallbackWrapper1_1<R, T>::l_call),
                force_cast<void*>(f));
    }
    template<typename R, typename T>
    luaClassRegister& method1_1(const char *fname, R(ClassT::*f)(T) const)
    {
        return _method(fname, 
                force_cast<lua_CFunction>(&LuaMethodCallbackWrapper1_1<R, T>::l_call),
                force_cast<void*>(f));
    }

    template<typename R>
    luaClassRegister& method1_0(const char *fname, R(ClassT::*f)() const)
    {
        return _method(fname, 
                force_cast<lua_CFunction>(&LuaMethodCallbackWrapper1_0<R>::l_call),
                force_cast<void*>(f));
    }

    ~luaClassRegister()
    {
        lua_pop(m_l, 2);
    }

private:
    luaClassRegister& _method(const char *fname, lua_CFunction cf, void *f)
    {
        lua_pushlightuserdata(m_l, f);
        lua_pushstring(m_l, m_className.c_str());
        lua_pushcclosure(m_l, cf, 2);
        lua_setfield(m_l, -2, fname);
        return *this;
    }

    static int l_destruct(lua_State* l)
    {
        const char *className = lua_tostring(l, lua_upvalueindex(1));
        ClassT* p = static_cast<ClassT*>(luaL_checkudata(l, 1, className));
        p->~ClassT();
        return 0;
    }
    template<typename T>
    static int l_construct(lua_State* l)
    {
        const char *className = lua_tostring(l, lua_upvalueindex(1));

        typename VariableType<T>::Type val;
        luaStack_get(l, 1, val);
        ClassT *p = new (lua_newuserdata(l, sizeof(ClassT))) ClassT(val);
        luaL_getmetatable(l, className);
        lua_setmetatable(l, -2);
        return 1;
    }

private:
    std::string m_className;
    lua_State *m_l;
};

class LuaTable
{
public:
    LuaTable():
        m_stackIdx(-1), m_ref(LUA_NOREF), m_l(NULL){}
    LuaTable(lua_State *l):
        m_stackIdx(-1), m_ref(LUA_NOREF), m_l(l)
    {
        lua_newtable(m_l);
        m_ref = luaL_ref(m_l, LUA_REGISTRYINDEX);
    }
    LuaTable(lua_State *l, int stackIdx):
        m_stackIdx(-1), m_ref(LUA_NOREF), m_l(l)
    {
        lua_pushvalue(m_l, stackIdx);
        assert(lua_istable(m_l, lua_gettop(m_l)));
        m_ref = luaL_ref(m_l, LUA_REGISTRYINDEX);
    }
    LuaTable(lua_State *l, const char *tname):
        m_stackIdx(-1), m_ref(LUA_NOREF), m_l(l)
    {
        lua_getglobal(m_l, tname);
        assert(lua_istable(m_l, lua_gettop(m_l)));
        m_ref = luaL_ref(m_l, LUA_REGISTRYINDEX);
    }
    LuaTable(const LuaTable& o):
        m_stackIdx(-1), m_ref(LUA_NOREF), m_l(o.m_l)
    {
        lua_rawgeti(m_l, LUA_REGISTRYINDEX, o.m_ref);
        m_ref = luaL_ref(m_l, LUA_REGISTRYINDEX);
    }
    LuaTable& operator = (const LuaTable& o)
    {
        LuaTable t(o);
        t.swap(*this);
        return *this;
    }
    ~LuaTable()
    {
        assert(m_stackIdx == -1);
        if (m_ref != LUA_NOREF) {
            luaL_unref(m_l, LUA_REGISTRYINDEX, m_ref);
        }
    }
    void swap(LuaTable& o)
    {
        std::swap(m_ref, o.m_ref);
        std::swap(m_stackIdx, o.m_stackIdx);
        std::swap(m_l, o.m_l);
    }

    template<typename KeyT, typename ValueT>
    void get(KeyT k, ValueT &v) const
    {
        bool accessing = m_stackIdx != -1;
        if (!accessing) beginAccess();

        luaStack_push(m_l, k);
        lua_gettable(m_l, m_stackIdx);
        luaStack_get(m_l, lua_gettop(m_l), v);
        lua_pop(m_l, 1);

        if (!accessing) endAccess();
    }

    template<typename KeyT, typename ValueT>
    void set(KeyT k, ValueT v)
    {
        bool accessing = m_stackIdx != -1;
        if (!accessing) beginAccess();

        luaStack_push(m_l, k);
        luaStack_push(m_l, v);
        lua_settable(m_l, m_stackIdx);

        if (!accessing) endAccess();
    }

    template<typename ValueT>
    void geti(int i, ValueT &v) const
    {
        bool accessing = m_stackIdx != -1;
        if (!accessing) beginAccess();

        lua_rawgeti(m_l, m_stackIdx, i);
        luaStack_get(m_l, lua_gettop(m_l), v);
        lua_pop(m_l, 1);

        if (!accessing) endAccess();
    }

    template<typename ValueT>
    void seti(int i, ValueT v)
    {
        bool accessing = m_stackIdx != -1;
        if (!accessing) beginAccess();

        luaStack_push(m_l, v);
        lua_rawseti(m_l, m_stackIdx, i);

        if (!accessing) endAccess();
    }

    int length() const
    {
        bool accessing = m_stackIdx != -1;
        if (!accessing) beginAccess();

        size_t len = lua_objlen(m_l, m_stackIdx);

        if (!accessing) endAccess();
        return static_cast<int>(len);
    }

    void beginAccess()  const
    {
        lua_rawgeti(m_l, LUA_REGISTRYINDEX, m_ref);
        m_stackIdx = lua_gettop(m_l);
    }
    void endAccess() const
    {
        lua_remove(m_l, m_stackIdx);
        m_stackIdx = -1;
    }

    void push2Stack() const
    {
        lua_rawgeti(m_l, LUA_REGISTRYINDEX, m_ref);
    }

private:
    lua_State *m_l;
    int m_ref;
    mutable int m_stackIdx;
};

void test0(int i)
{
    puts("call back begin");
    cout << i << endl;
    puts("call back end");
}
void test1(const std::string& i)
{
    puts("call back begin");
    cout << i << endl;
    puts("call back end");
}
void test2(const char *i)
{
    puts("call back begin");
    cout << i << endl;
    puts("call back end");
}
int test3(int i)
{
    int r = 0;
    puts("call back begin");
    r = i * i * i;
    puts("call back end");
    return r;
}

class Array
{
public:
    Array(int n): m_buf(n)
    {
        puts("construct Array");
    }
    ~Array()
    {
        puts("destruct Array");
    }
    int get(int i) const { return m_buf[i]; }
    void set(int i, int val) { m_buf[i] = val; }
    int size() const { return (int)m_buf.size(); }

private:
    std::vector<int> m_buf;
};

void testArray(Array* a)
{
    for (int i = 0; i < a->size(); ++i) {
        printf("%d, ", a->get(i));
    }
    puts("");
}

void printTable(const LuaTable& t)
{
    t.beginAccess();
    int  l = t.length();
    for (int i = 1; i <= l; ++i) {
        int v = 0;
        t.geti(i, v);
        printf("%d,", v);
    }
    t.endAccess();
    puts("");
}

LuaTable buildTable()
{
    LuaTable t(getLuaState());
    t.seti(1, 2);
    t.set("a", "sdf");
    return t;
}

lua_State *g_state;
lua_State* getLuaState()
{
    return g_state;
}

int main()
{
    lua_State *l = lua_open();
    luaL_openlibs(l);

    g_state = l;

    luaL_loadfile(l, "init.lua");
    lua_pcall(l, 0, 0, 0);

    luaCall0_1(l, "print", 3);
    luaCall0_1(l, "print", 5.4);
    luaCall0_1(l, "print", "jfkdsl");
    luaCall0_1(l, "print", std::string("hello world"));
    cout << luaCall1_1<float>(l, "myFunc", 3.14) << endl;

    {
        LuaStackBalanceKeeper keeper(l);

        lua_newtable(l);
        lua_setglobal(l, "scan_table");

        LuaTable t(l, "scan_table");
        t.beginAccess();
        for (int i = 1; i < 10; ++i) t.seti(i, i * i);
        t.endAccess();
    }

    luaRegisterFunction0_1(l, "test0", &test0);
    luaRegisterFunction0_1(l, "test1", &test1);
    luaRegisterFunction0_1(l, "test2", &test2);
    luaRegisterFunction1_1(l, "test3", &test3);
    luaRegisterFunction0_1(l, "testArray", &testArray);
    luaRegisterFunction0_1(l, "printTable", &printTable);
    luaRegisterFunction1_0(l, "buildTable", &buildTable);

    luaClassRegister<Array>(l, "Array")
        .constructor<int>()
        .method0_2<int, int>("__newindex", &Array::set)
        .method1_1<int, int>("__index", &Array::get)
        .method1_0<int>("__len", &Array::size);

    char buf[128] = "";
    while (gets(buf)) {
        int err = luaL_loadstring(l, buf) | lua_pcall(l, 0, 0, 0);
        if (err) {
            printf("lua err : %s\n", lua_tostring(l, -1));
            lua_pop(l, 1);
        }
    }

    lua_close(l);
}
