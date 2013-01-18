#pragma once

#define BEGIN_TEST(name)    \
    static  \
    class __TestModel_ ## name  \
    {   \
    public: \
        __TestModel_ ## name()  \
        {   \
            void(*funcs[])() =  \
            {               

#define TEST(func)  &func,

#define END_TEST(name)  \
            };  \
            for (size_t i = 0; i < sizeof(funcs) / sizeof(funcs[0]); ++i) funcs[i]();   \
        }   \
    } __g_testModel_ ## name;