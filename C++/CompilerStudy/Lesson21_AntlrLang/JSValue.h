
#ifndef JS_VALUE_H
#define JS_VALUE_H

struct GCObject;
struct JSArray;
struct JSString;
struct Function;

enum JSValueType {
    JSVT_Nil,
    JSVT_Boolean,
    JSVT_Number,
    JSVT_String,
    JSVT_Array,
    JSVT_Function,
};

#undef TRUE
#undef FALSE

struct JSValue {
    union {
        JSString *str;
        JSArray *array;
        Function *func;
        double num;
        bool b;
    } data;
    JSValueType type;

    bool isNil() const { return type == JSVT_Nil; }
    bool getBoolean() const {
        if (type == JSVT_Nil || (type == JSVT_Boolean && !data.b)) return false;
        return true;
    }
    GCObject* gcAccess() const;
    string toString() const;

    JSValue(): type(JSVT_Nil){}
    static JSValue fromBoolean(bool b){ JSValue r; r.type = JSVT_Boolean; r.data.b = b;  return r;}
    static JSValue fromNumber(double num) { JSValue r; r.type = JSVT_Number; r.data.num = num;  return r;}
    static JSValue fromString(const char *str);
    static JSValue fromArray(JSArray *array) { JSValue r; r.type = JSVT_Array; r.data.array = array;  return r;}
    static JSValue fromFunction(Function *func) { JSValue r; r.type = JSVT_Function; r.data.func = func;  return r;}

    bool operator == (const JSValue& o) const;
    bool operator != (const JSValue& o) const { return !(*this == o);}

    static JSValue NIL;
    static JSValue TRUE;
    static JSValue FALSE;
};

namespace std {
template<>
struct hash<JSValue> {
    int operator () (const JSValue& v) const {
        switch (v.type) {
            case JSVT_Nil: ASSERT(0);
            case JSVT_Boolean: return (int)std::hash<bool>()(v.data.b);
            case JSVT_Number: return (int)std::hash<double>()(v.data.num);
            case JSVT_String: return (int)std::hash<JSString*>()(v.data.str);
            case JSVT_Array: return (int)std::hash<JSArray*>()(v.data.array);
            case JSVT_Function: return (int)std::hash<Function*>()(v.data.func);
            default: ASSERT(0);
        }
        return 0;
    }
};
}

inline bool JSValue::operator == (const JSValue& o) const {
    if (type != o.type) return false;
    switch (type) {
        case JSVT_Nil: return true;
        case JSVT_Boolean: return data.b == o.data.b;
        case JSVT_Number: return data.num == o.data.num;
        case JSVT_String: return data.str == o.data.str;
        case JSVT_Array: return data.array == o.data.array;
        case JSVT_Function: return data.func == o.data.func;
        default: ASSERT(0);
    }
}

#endif
