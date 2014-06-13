#include "pch.h"
#include "SchemeVM.h"
#include "SchemeMemoryManager.h"

SchemeVM::SchemeVM() {
    mMemMgr = new SchemeMemoryManager();
    mSymPool = new SchemeSymbolPool();
    setupEnvG();
}

SchemeVM::~SchemeVM() {
    mEnvG = nullptr;
    DELETE(mSymPool);
    DELETE(mMemMgr);
}

void SchemeVM::setupEnvG() {
}

SchemeRef SchemeVM::_eval(SchemeEnv *env, const SchemeRef &exp) {
    // TODO
    return SchemeRef(1);
}
