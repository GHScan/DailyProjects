#ifndef UNIT_TEST_H
#define UNIT_TEST_H

#include <stdio.h>

#include <Utility.h>
#include <Assert.h>

int by_matchPattern(const char *pat, const char *str);

#define by_TEST_CASE_BEGIN(func)     void func(const char *__pat) { \
    if (!by_matchPattern(__pat, #func)) return;\
    static int by_UNIQ_VAR_NAME(l_executed) = 0;\
    if (by_UNIQ_VAR_NAME(l_executed)) return;\
    by_UNIQ_VAR_NAME(l_executed) = 1;

#define by_TEST_CASE_DRIVER(pat)   const char *__pat = pat;
#define by_TEST_CASE_DEPENDENT(externFunc)     { extern void externFunc(const char*);  externFunc(__pat); }

#define by_TEST_CASE_END()  printf("@%s Ok!", by_FUNCTION); }

#endif
