#ifndef SPROTO_H
#define SPROTO_H

struct SFuncProto {
    int codeSize;
    uint8_t *codes;
};

struct SClassProto {
    struct FieldInfo {
        Atom *name;
    };
    struct MethodInfo {
        Atom *name;
        SFuncProto *funcProto;
    };

    int fieldCount;
    FieldInfo *fieldInfos;
    int methodCount;
    MethodInfo *methodInfos;
};

#endif
