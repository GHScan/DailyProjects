
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <ctype.h>
#include <string>
#include <iostream>
using namespace std;

int ch2int(char c) {
    c = tolower(c);
    if (c >= '0' && c <= '9') {
        return c - '0';
    } else if (c >= 'a' && c <= 'f') {
        return c - 'a' + 10;
    } else {
        assert(0);
    }
}

int main(int argc, char *argv[]) {
    int n = 1;
    for (int i = 1; i < argc; ++i) {
        if (argv[i][0] == '-' && isdigit(argv[i][1])) {
            n = atoi(argv[i] + 1);
            break;
        }
    }

    string out;
    for (string line; getline(cin, line); ) {
        for (int i = 0; i < (int)line.size(); ++i) {
            if (isalnum(line[i])) {
                out += (ch2int(line[i]) << 4) | ch2int(line[i + 1]);
                ++i;
            }
        }
    }
    for (int i = 0; i < n; ++i) cout << out;
}
