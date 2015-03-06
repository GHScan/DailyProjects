#include "stdafx.h"

//------------------------------
#include <limits>

class Solution {
public:
    double pow(double x, int n) {
        if (n == 0) return 1;
        else if (n == numeric_limits<int>::min()) {
            return 1 / (pow(x, numeric_limits<int>::max()) * x);
        } 
        else if (n < 0) return 1 / pow(x, -n);
        else if (n % 2 == 0) {
            double r = pow(x, n / 2);
            return r * r;
        }
        else {
            return pow(x, n - 1) * x;
        }
    }
};

//------------------------------
int main() {
    Solution s;
    double xs[] = { -3.2, -2.6, -1.0, 0, 1, 3, 11, };
    int ns[] = { -3, -2, 0, 1, 2, 10, 100, };
    for (auto x : xs) {
        for (auto n : ns) {
            double r1 = s.pow(x, n);
            double r2 = ::pow(x, n);
            if (x == 0 && n < 0);
            else assert(fabs(r1 - r2) <= 0.000000001 * fabs(r1));
        }
    }
    assert(s.pow(1.00000, -2147483647 - 1) == 1);
}
