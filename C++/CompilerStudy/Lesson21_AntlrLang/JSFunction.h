
#ifndef JS_FUNCTION_H
#define JS_FUNCTION_H

struct Function {
    enum FuncType {
        FT_JS,
        FT_C,
    };
    FuncType type;
};

struct FuncMeta {
};
typedef shared_ptr<FuncMeta> FuncMetaPtr;

struct JSFunction:
    public Function {
    FuncMetaPtr meta;
};

struct CFunction:
    public Function {
};

#endif
