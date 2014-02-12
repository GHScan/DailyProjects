#include "pch.h" 

#include <lua5.1/lua.hpp>

static vector<string> split(const char *_str, const char *delim) {
    vector<string> r;
    string str(_str);
    char *p = strtok((char*)str.c_str(), delim);
    while (p != NULL) {
        r.push_back(p);
        p = strtok(NULL, delim);
    }
    return r;
}
static string readFile(const char *fname) {
    string r;
    FILE *f = fopen(fname, "rb");
    if (f == NULL) return r;
    fseek(f, 0, SEEK_END);
    r.resize(ftell(f) + 1);
    fseek(f, 0, SEEK_SET);
    fread((char*)r.c_str(), r.size(), 1, f);
    r.back() = 0;
    fclose(f);
    return r;
}

class LuaConfig {
public:
    LuaConfig(const char *src) {
        m_L = lua_open();
        luaL_openlibs(m_L);
        if (luaL_loadstring(m_L, src) || !_stackCall(0, 0)) {
            _stackReportError();
        }
    }
    ~LuaConfig() {
        lua_close(m_L);
    }

    template<typename T>
    T get(const char *path) {
        _stackPushPath(split(path, "."), 0, -1);
        T r;
        _stackGet(r);
        lua_pop(m_L, 1);
        return r;
    }
    template<typename T>
    void set(const char *path, const T& val) {
        vector<string> names = split(path, ".");
        _stackPushPath(names, 0, -2);
        _stackSet(names.back().c_str(), val);
    }
    void call(const char *path) {
        _stackPushPath(split(path, "."), 0, -1);
        if (!_stackCall(0, 0)) _stackReportError();
    }
private:
    void _stackPushPath(const vector<string>& names, int begin, int end) {
        if (end < 0) end += (int)names.size();
        lua_getglobal(m_L, "_G");
        for (; begin <= end; ++begin) {
            lua_getfield(m_L, -1, names[begin].c_str());
        }
    }
    bool _stackCall(int nargs, int nres) {
        return lua_pcall(m_L, nargs, nres, 0) == 0;
    }
    void _stackReportError() {
        fprintf(stderr, "%s", lua_tostring(m_L, -1));
        lua_pop(m_L, 1);
    }

    void _stackGet(int &val) { val = luaL_checkinteger(m_L, -1); }
    void _stackGet(float &val) { double d; _stackGet(d); val = (float)d; }
    void _stackGet(double &val) { val = luaL_checknumber(m_L, -1); }
    void _stackGet(long &val) { val = luaL_checklong(m_L, -1); }
    void _stackGet(string &val) { val = luaL_checkstring(m_L, -1); }

    void _stackSet(const char *name, int val) { lua_pushinteger(m_L, val); lua_setfield(m_L, -2, name); }
    void _stackSet(const char *name, float val) { lua_pushnumber(m_L, val); lua_setfield(m_L, -2, name); }
    void _stackSet(const char *name, double val) { lua_pushnumber(m_L, val); lua_setfield(m_L, -2, name); }
    void _stackSet(const char *name, long val) { lua_pushinteger(m_L, val); lua_setfield(m_L, -2, name); }
    void _stackSet(const char *name, const string &val) { lua_pushstring(m_L, val.c_str()); lua_setfield(m_L, -2, name); }
private:
    lua_State *m_L;
};

int main() {
    LuaConfig config(readFile("config.lua").c_str());

    puts("@get configs:");
    printf("\tname=%s\n\tfilecount=%d\n\tconfig.version=%f\n",
            config.get<string>("name").c_str(),
            config.get<int>("filecount"),
            config.get<float>("config.version")
            );

    puts("@set configs:");
    config.set("config.version", 4.2);
    printf("\tconfig.version -> %f\n", config.get<float>("config.version"));

    puts("@call actions:");
    config.call("config.action");
}
