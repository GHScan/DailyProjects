#include <stdio.h>
#include <stdlib.h>

#include <iostream>
using namespace std;

#include "BitVector.h"
#include "Timer.h"

int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage : %s n\n", argv[0]);
        return 1;
    }

    vector<int> nums;
    for (int n; cin >> n;) nums.push_back(n);

    {
        Timer _t(stderr, "bitvector");

        BitVector v(atoi(argv[1]));
        for (auto i : nums) v.set(i, 1);
        nums.clear();
        for (int i = 0; i < v.getSize(); ++i) {
            if (v.get(i)) nums.push_back(i);
        }
    }

    for (auto i : nums) cout << i << '\n';

    return 0;
}
