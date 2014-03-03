#include <stdio.h>
#include <time.h>

#include <vector>
#include <algorithm>
using namespace std;

int main(int argc, char *argv[]) {
    if (argc < 3) {
        fprintf(stderr, "Usage : %s n k\n", argv[0]);
        return 1;
    }

    int n = atoi(argv[1]);
    int k = atoi(argv[2]);
    if (n < 0 || k > n) {
        fprintf(stderr, "Invalid args\n");
        return 1;
    }

    srand(time(NULL));

    vector<int> nums;
    for (int i = 0; i < n; ++i) nums.push_back(i);
    random_shuffle(nums.begin(), nums.end());

    for (int i = 0; i < k; ++i) printf("%d\n", nums[i]);

    return 0;
}
