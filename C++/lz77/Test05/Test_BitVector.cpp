#include "StdAfx.h"

#include "BitVector.h"

#ifdef UNIT_TEST

namespace
{

class Test_BitVector
{
public:
    Test_BitVector()
    {
        cout << "Test_BitVector begin: " << endl;
        test_syntax();
        cout << "Test_BitVector end: " << endl;
    }

private:
    void test_syntax()
    {
        (BitVector());

        BitVector a(15);
        BitVector b(1, 32);

        assert(a.get<int>(0) == 15);
        assert(b.get<int>(0) == -1);

        a.push_back(16);
        assert(a.get<int>(32) == 16);
        b.push_back(true);
        assert(b.size() == 33);
        
        b.pop_back(25);
        assert(b.size() == 8);
        assert(b.get<char>(0) == char(-1));

        a.clear();

        int ia = 155, ib = 23445;
        a.assign(ia), b.assign(ib);

        ia &= ib, a.and(b);
        assert(a.get<int>(0) == ia);

        ib |= ia, b.or(a);
        assert(b.get<int>(0) == ib);

        ia ^= ib, a.xor(b);
        assert(a.get<int>(0) == ia);

        ib = ~ib, b.not();
        assert(b.get<int>(0) == ib);
    }

private:
}g_testBitVector;

} // namespace

#endif