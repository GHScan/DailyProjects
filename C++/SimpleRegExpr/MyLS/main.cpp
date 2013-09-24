
#include <stdint.h>
#include <math.h>
#include <string.h>
#include <time.h>
#include <assert.h>

#include <string>
#include <vector>
#include <iostream>
#include <typeinfo>
using namespace std;

bool match(const char *pat, const char *str) {
    if (pat[0] == 0) return str[0] == 0;
    if (pat[0] == '*') {
        do {
            if (match(pat + 1, str)) return true;
        } while(*str++);
        return false;
    }
    return pat[0] == str[0] && match(pat + 1, str + 1);
}

#include <dirent.h> 
void listfile(const char *dir, const char *pat) {
    DIR *h = opendir(dir);
    dirent *entry;
    if (h == NULL) return;
    while ((entry = readdir(h)) != NULL) {
        if (match(pat, entry->d_name)) printf("%s\n", entry->d_name);
    }
    closedir(h);
}

void benchmark() {
    const char *testCase[][3] = {
        {"abb*jk*s", "abbfdsjkajs", "",},
        {"*", "fsjk23fjk89d", "",},
        {"fs*dd", "fsjk23fjk89d", NULL,},
        {"*23dfjk*", "bbjkaalfdjk", NULL,},
        {"*23dfjk*", "fjks23dfjk923", "",},
    };

    const int LOOP = 2 * 1000 * 1000;

    clock_t start = clock();
    for (int i = 0; i < LOOP; ++i) {
        for (int i = 0; i < sizeof(testCase) / sizeof(testCase[0]); ++i) {
            if (match(testCase[i][0], testCase[i][1]) != (testCase[i][2] != NULL)) puts("error");
        }
    }
    printf("benchmark : %f sec\n", float(clock() - start) / CLOCKS_PER_SEC);
}

int main() {
    // benchmark();

    for (string line; ;) {
        printf(">>> ");
        getline(cin, line);
        string dir(".");
        string pat(line);
        if (const char* p = strrchr(line.c_str(), '/')) {
            dir = string(line.c_str(), p);
            pat = string(p + 1, line.c_str() + line.size());
        }
        listfile(dir.c_str(), pat.c_str());
    }
}
