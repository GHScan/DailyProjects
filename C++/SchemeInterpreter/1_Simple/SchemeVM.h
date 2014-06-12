#ifndef SCHEME_VM_H
#define SCHEME_VM_H

#include "SchemeSymbol.h"

class SchemeMemoryManager;

class SchemeVM {
public:
    enum {
        SYM_None,
        SYM_If,
        SYM_Begin,
        SYM_Lambda,
        SYM_Quote,
        SYM_Quasiquote,
        SYM_Unquote,
        SYM_UnquoteSlicing,
        SYM_Define,
        SYM_Set,
    };

public:
    SchemeVM();
    SchemeVM(const SchemeVM &o) = delete;
    SchemeVM& operator = (const SchemeVM &o) = delete;
    ~SchemeVM();

    const SchemeSymbolPool* getSymbolPool() const {
        return mSymPool;
    }

    SchemeSymbolPool* getSymbolPool() {
        return mSymPool;
    }

private:
    SchemeSymbolPool *mSymPool;
    SchemeMemoryManager *mMemMgr;
};

#endif
