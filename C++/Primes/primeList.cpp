#include "pch.h"

#include <vector>

vector<int> primeList(int maxN)
{
    vector<bool> boolL(maxN + 1, true);
    boolL[0] = boolL[1] = false;
    for (int i = 2; i < maxN; ++i) {
        if (boolL[i]) {
            for (int j = i + i; j < maxN; j += i) {
                boolL[j] = false;
            }
        }
    }
    vector<int> r;
    for (int i = 2; i < maxN; ++i) if (boolL[i]) r.push_back(i);
    return r;
}

int main()
{
    cout << primeList(2000000).size() << endl;
}
