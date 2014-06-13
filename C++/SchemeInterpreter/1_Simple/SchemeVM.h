#ifndef SCHEME_VM_H
#define SCHEME_VM_H

#include "SchemeSymbol.h"
#include "SchemeRef.h"

class SchemeMemoryManager;
class SchemeEnv;

class SchemeVM {
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

    SchemeRef eval(const SchemeRef &exp) {
        return _eval(mEnvG, exp);
    }

private:
    void setupEnvG();

    SchemeRef _eval(SchemeEnv *env, const SchemeRef &exp);

private:
    SchemeSymbolPool *mSymPool;
    SchemeMemoryManager *mMemMgr;
    SchemeEnv *mEnvG;
};

#endif
