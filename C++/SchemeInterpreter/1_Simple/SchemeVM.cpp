#include "pch.h"
#include "SchemeVM.h"

SchemeVM::SchemeVM() {
    mSymPool = new SchemeSymbolPool();
    mSymPool->intern("if", SYM_If);
    mSymPool->intern("begin", SYM_Begin);
    mSymPool->intern("lambda", SYM_Lambda);
    mSymPool->intern("quote", SYM_Quote);
    mSymPool->intern("quasiquote", SYM_Quasiquote);
    mSymPool->intern("unquote", SYM_Unquote);
    mSymPool->intern("unquote-slicing", SYM_UnquoteSlicing);
    mSymPool->intern("define", SYM_Define);
    mSymPool->intern("set!", SYM_Set);
}

SchemeVM::~SchemeVM() {
    DELETE(mSymPool);
}
