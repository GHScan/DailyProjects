
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
    if (pat[0] == 0 && str[0] == 0) return true;
    if (pat[0] == '*') {
        for (; str[0]; ++str) {
            if (match(pat + 1, str)) return true;
        }
        return match(pat + 1, str);
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

int main() {
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
