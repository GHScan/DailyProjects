#include "stdafx.h"

#include <cassert>

#include "BitStream.h"

static void test_BitStream_0()
{
    BitStream bs;
    assert(bs.getBitCount() == 0);
    assert(bs.getPtr() == NULL);

    bs.push_back(false);
    assert(bs.getBitCount() == 1);
    assert(bs.getPtr() != NULL);

    bool b = false;
    for (int i = 0; i < 31; ++i)
    {
        bs.push_back(b = !b);
    }
    assert(bs.getBitCount() == 32);

    assert(*(int*)bs.getPtr() == 0x55555555);

    bs.rotateLeft(1);
    assert(*(int*)bs.getPtr() == 0xaaaaaaaa);

    bs.rotateRight(1);
    assert(*(int*)bs.getPtr() == 0x55555555);
    bs.rotateRight(1);
    assert(*(int*)bs.getPtr() == 0xaaaaaaaa);

    bs.rotateLeft(30);
    assert(*(int*)bs.getPtr() == 0xaaaaaaaa);
}

static void test_BitStream_1()
{
    int i = 0xfe543210;
    BitStream bs(&i, 32);

    assert(*(int*)bs.getPtr() == i);

    bs.pop_back(16);
    assert(*(short*)bs.getPtr() == 0x3210);

    bs.reverse(0, bs.getBitCount());
    assert(*(short*)bs.getPtr() == 0x084c);

    bs.write(4, 0);
    assert(*(short*)bs.getPtr() == 0x0844);
}

static void test_BitStream_2()
{
    char i = 0xef;
    BitStream bs0(&i, 8);
    BitStream bs1(&i, 4);
    BitStream bs2(&i, 2, 4);
    
    BitStream bs3;
    for (int i = 0; i < 16; ++i)
    {
        bs3.push_back(i & 1);
    }

    BitStream bs4 = bs0;
    bs4 += bs1;
    bs4 += bs2;
    bs4 += bs3;

    assert(*(int*)bs4.getPtr() == 0x5555ebef);

    BitStream bs5;
    for (int i = 0; i < 10; ++i)
    {
        bs5 += bs4;
    }
    for (int i = 0; i < 10; ++i)
    {
        assert(((int*)bs5.getPtr())[i] == 0x5555ebef);
    }
}

static void test_BitStream_3()
{
    int i = 0xabcd;
    BitStream bs;
    bs.push_front(&i, 16);

    i = 0x1234;
    bs.push_front(&i, 16);

    assert(((int*)bs.getPtr())[0] == 0xabcd1234);
}

static void test_BitStream_4()
{
    int i = 0x1234fedc;
    BitStream bs(&i, 32);
    BitStreamIterator iter(bs);
    BitStream bs2;
    bs2.pushN0(32);
    BitStreamIterator iter2(bs2);
    while (iter.hasMore())
    {
        iter2.writeNext(iter.readNext());
    }
    assert(((int*)bs2.getPtr())[0] == i);
}

static void test_BitStream_5()
{
    long long i = 0xabcd1234;
    i <<= 32;
    i |= 0x12344321;

    BitStream bs(&i, 64);
    std::string total = bs.toString();
    for (int i = 0; i < 8; ++i)
    {
        BitStream bs_(bs.getPtr(), i, i * i + i);
        if (total.find(bs_.toString()) == -1)
        {
            cout << total << endl;
            cout << bs_.toString() << endl;
        }
        assert(total.find(bs_.toString()) != -1);
    }

    i = 0x11223344;
    i <<= 32;
    i |= 0xfeefccbb;
    bs.pop_back(64);
    bs.push_back(&i, 64);
    total = bs.toString();
    for (int i = 0; i < 8; ++i)
    {
        BitStream bs_(bs.getPtr(), i, i * i + i);
        if (total.find(bs_.toString()) == -1)
        {
            cout << total << endl;
            cout << bs_.toString() << endl;
        }
        assert(total.find(bs_.toString()) != -1);
    }

    i = 0xf987ecaa;
    i <<= 32;
    i |= 0x13245678;
    bs.pop_back(64);
    bs.push_back(&i, 64);
    total = bs.toString();
    for (int i = 0; i < 8; ++i)
    {
        BitStream bs_(bs.getPtr(), i, i * i + i);
        if (total.find(bs_.toString()) == -1)
        {
            cout << total << endl;
            cout << bs_.toString() << endl;
        }
        assert(total.find(bs_.toString()) != -1);
    }
}

void test_BitStream()
{
    test_BitStream_0();
    test_BitStream_1();
    test_BitStream_2();
    test_BitStream_3();
    test_BitStream_4();
    test_BitStream_5();
}

class Test_BitStream
{
public: 
    Test_BitStream()
    {
        test_BitStream();
    }
};
#ifdef _DEBUG
static Test_BitStream  g_test;
#endif