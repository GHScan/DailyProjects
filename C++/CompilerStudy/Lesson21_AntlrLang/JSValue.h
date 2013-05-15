
#ifndef JS_VALUE_H
#define JS_VALUE_H

struct JSArray;
struct JSString;
struct Function;

enum  JSValueType {
    JSVT_Boolean,
    JSVT_Number,
    JSVT_String,
    JSVT_Array,
    JSVT_Function,
};
struct JSValue {
    union {
        JSString *str;
        JSArray *array;
        Function *func;
        double num;
        bool b;
    } data;
    JSValueType type;
};

#endif
