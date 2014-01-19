#include "pch.h" 

#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <assert.h>

#include <string>
#include <vector>
using namespace std;

int main(int argc, char *argv[]) {
    if (argc < 5) {
        printf("Usage : %s chars n minsize maxsize", argv[0]);
        return 0;
    }

    setlocale(LC_ALL, "");

    srandom(time(NULL));

    const char *chars = argv[1];
    int charslen = strlen(chars);
    int n = atoi(argv[2]);
    int minsize = atoi(argv[3]), maxsize = atoi(argv[4]);
    maxsize = max(maxsize, minsize);

    string str(maxsize + 1, 0);
    while (--n >= 0) {
        int size = random() % (maxsize - minsize + 1) + minsize;
        for (int i = 0; i < size; ++i) str[i] = chars[random() % charslen];
        str[size] = 0;
        cout << str.c_str() << '\n';
    }
}
