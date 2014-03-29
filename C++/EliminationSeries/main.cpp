#include "pch.h"

static void place(int *p, int n) {
    for (int i = 1; i <= n; i *= 2) p[i] = 1;

    int power2Ceil = 1;
    for (int i = 2; i <= n; ++i) {
        if ((i & (i - 1)) == 0) power2Ceil *= 2;
        if (p[i] != 0) continue;

        int v = power2Ceil + 1 - p[i - 1];
        for (int j = i; j <= n; j *= 2) {
            p[j] = v;
        }
    }
}

int main() {
    for (;;) {
        puts("input team number:(power of 2)");
        int n;
        scanf("%d", &n);
        if ((n & (n - 1)) != 0) {
            puts("must be power of 2");
            continue;
        }
        n = n * 2 - 1;

        vector<int> v(n, 0);
        place(&v[0] - 1, n);

        for (int i = 1; i <= n; ++i) {
            if ((i & (i - 1)) == 0) puts("");
            printf("%-3d", v[i - 1]);
        }
        puts("");
    }
}
