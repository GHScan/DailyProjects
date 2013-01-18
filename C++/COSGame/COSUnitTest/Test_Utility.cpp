#include "stdafx.h"

#include <fstream>

#include <boost/filesystem.hpp>

#include <windows.h>

#include "../COSFoundationLib/Utility.h"

#include "Tests.h"

namespace Scan
{

static void test_utility_force_cast()
{
    {
        struct A{ int i; };
        A a = force_cast<A>(3);
        ASSERT(a.i == 3);
    }

    {
        int i = 3;
        int *p = force_cast<int*>(i);
        ASSERT(p == (void*)3);
        int j = force_cast<int>(p);
        ASSERT(j == 3);
    }
}

static void test_utility_arraySize()
{
    struct A{};
    A a[5];
    ASSERT(sizeOfArray(a) == 5);
}

static void test_utility_toString()
{
    ASSERT(toString(3) == "3");
    ASSERT(toString('c') == "c");
    ASSERT(toString("abc") == "abc");
    ASSERT(toString(true) == "true");
}

static void test_utility_fromString()
{
    bool b;
    ASSERT(fromString(b, "false") && !b);
    ASSERT(fromString(b, "true") && b);
    ASSERT(fromString(b, "fd") && !b);  // 也就是说, 对于bool永成功, 且只有"true"的时候读取true

    String s;
    ASSERT(fromString(s, "false") && s == "false");

    char c;
    ASSERT(fromString(c, "A") && c == 'A');
    ASSERT(!fromString(c, "sdf"));  // 长度过长

    int i;
    ASSERT(fromString(i, "564") && i == 564);
    ASSERT(!fromString(i, "f564"));
    // ASSERT(!fromString(i, "564f"));
}   

static void test_utility_NativeLocale()
{
    String name = "呵呵.txt";

    {
        NativeLocale l;
        std::ofstream fo(name.c_str());
    }

    {
        NativeLocale l;
        std::ifstream fi(name.c_str());
        ASSERT(fi);
    }

    {
        NativeLocale l;
        boost::filesystem::remove(name);
        // 确保完全删除掉了
        ASSERT(!boost::filesystem::exists(name));
    }
}

static void test_utility_TypeInfoWrapper()
{
    TypeInfoWrapper t_int(typeid(int));
    TypeInfoWrapper t_int1(typeid(3));
    TypeInfoWrapper t_float(typeid(3.0f));

    ASSERT(t_int == t_int1);
    ASSERT(t_int != t_float);
}

static void test_utility_StringPrinter()
{
    String name = "1.txt";

    StringPrinter("abc\n").printToFile(name);
    StringPrinter().format("abc").format("%s\n", "def").printToFile(name);
    StringPrinter().pushFormat("abc").pushFormat("%s\n", "def").printToFile(name);
    {
        StringPrinter p;
        p.stream() << "ab" << "c";
        p.stream() << "de" << "f\n";
        p.printToFile(name);
    }
    {
        StringPrinter p;
        p.pushStream() << "ab" << "c";
        p.pushStream() << "de" << "f\n";
        p.printToFile(name);
    }

    {
        CachedOutputFileManager::getSingletonPtr()->getFile(name).flush();

        std::ifstream fi(name.c_str());
        const char *strs[] = 
        {
            "abc", "def", "abcdef", "def", "abcdef",
        };

        for (size_t i = 0; i < sizeOfArray(strs); ++i)
        {
            String l;
            ASSERT(getline(fi, l) && l == strs[i]);
        }
    }
}

static void test_utility_Timer()
{
    Timer t;
    float e = 0;
    ::Sleep(1000);
    e = t.getElapseSeconds();
    ASSERT(e > 0.9f && e < 1.1f);
    ::Sleep(1000);
    e = t.getElapseSeconds();
    ASSERT(e > 1.8f && e < 2.2f);
    t.reset();
    ::Sleep(1000);
    e = t.getElapseSeconds();
    ASSERT(e > 0.9f && e < 1.1f);
}

BEGIN_TEST(test_utility)
TEST(test_utility_force_cast)
TEST(test_utility_arraySize)
TEST(test_utility_fromString)
TEST(test_utility_toString)
TEST(test_utility_NativeLocale)
TEST(test_utility_TypeInfoWrapper)
TEST(test_utility_StringPrinter)
TEST(test_utility_Timer)
END_TEST(test_utility)

}