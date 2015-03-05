#include "stdafx.h"

//--------------------------------------------------
#include <limits>

class Solution {
public:
    int divide(int dividend, int divisor) {
        if (divisor == 0) {
            if (dividend == 0) return 1;
            else if (dividend > 0) return numeric_limits<int>::max();
            else return numeric_limits<int>::min();
        }

        unsigned int absDividend = dividend >= 0 ? dividend : (~dividend + 1);
        unsigned int absDivisor = divisor >= 0 ? divisor : (~divisor + 1);
        if (absDividend < absDivisor) return 0;

        bool negative = ((dividend ^ divisor) >> 31) != 0;
        unsigned int rAbs = divideAbs(absDividend, absDivisor);
        return negative ? (~rAbs + 1) : (rAbs < (unsigned int)numeric_limits<int>::max() ? rAbs : numeric_limits<int>::max());
    }
private:
    unsigned int divideAbs(unsigned int dividend, unsigned int divisor) {
        assert(dividend >= divisor && divisor != 0);

        int diff = maxBit(dividend) - maxBit(divisor);

        unsigned int a = dividend;
        unsigned int b = divisor << diff;
        unsigned int result = 0;
        for (int i = 0; i <= diff; ++i, b >>= 1) {
            result <<= 1;
            if (a >= b) {
                a -= b;
                result |= 1;
            }
        }
        return result;
    }
    int maxBit(unsigned int n) {
        int len = 0;
        for (; n > 0; n >>= 1, ++len);
        return len;
    }
};
//--------------------------------------------------

int main() {
    Solution s;

    int a[] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, numeric_limits<int>::max(), numeric_limits<int>::min(), };
    int b[] = { 1, 2, 3, 4, 5, numeric_limits<int>::max(), numeric_limits<int>::min(), };
    for (auto i : a) {
        for (auto j : b) {
            int negI = i == numeric_limits<int>::min() ? i : -i;
            int negJ = j == numeric_limits<int>::min() ? j : -j;

            assert(i / j == s.divide(i, j));
            assert(negI / j == s.divide(negI, j));
            if (!(negJ == -1 && i == numeric_limits<int>::min()))
                assert(i / negJ == s.divide(i, negJ));
            if (!(negJ == -1 && negI == numeric_limits<int>::min()))
                assert(negI / negJ == s.divide(negI, negJ));
        }
    }

    assert(s.divide(3, 0) == numeric_limits<int>::max());
    assert(s.divide(-3, 0) == numeric_limits<int>::min());
    assert(s.divide(numeric_limits<int>::min(), -1) == numeric_limits<int>::max());
}
