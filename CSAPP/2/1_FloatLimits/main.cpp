
#include <stdio.h>
#include <time.h>

#include <algorithm>
#include <iostream>
#include <vector>

using namespace std;

#define FLOAT_T float
//#define FLOAT_T double

void printSum(const vector<FLOAT_T> &v) {
    FLOAT_T sum = accumulate(v.begin(), v.end(), FLOAT_T(0), [](FLOAT_T a, FLOAT_T b){ return a + b; });
    printf("sum=%12f\n", sum);
}

int main() {
    srand(time(NULL));

    vector<FLOAT_T> v;
    for (int i = 10000; i > 1; --i) v.push_back(FLOAT_T(1) / i);

    printSum(v);

    v.push_back(10000000);
    printSum(v);

    reverse(v.begin(), v.end());
    printSum(v);

    random_shuffle(v.begin(), v.end());
    printSum(v);
}
