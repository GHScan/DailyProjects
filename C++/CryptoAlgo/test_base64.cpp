
#include "pch.h"
#include "base64.h"

void test_base64() {
    string datas[][2] = {
        {"a", "YQ=="},
        {"aa", "YWE="},
        {"aaa", "YWFh"},
        {"aaaa", "YWFhYQ=="},
        {"ab", "YWI="},
        {"abc", "YWJj"},
        {"abcd", "YWJjZA=="},
        {"=", "PQ=="},
        {"==", "PT0="},
        {"===", "PT09"},
        {"====", "PT09PQ=="},
        {"=====", "PT09PT0="},
        {"======", "PT09PT09"},
    };

    Base64 base64;
    for (auto &data : datas) {
        string e = base64.encode(data[0]);
        assert(e == data[1]);
        assert(base64.decode(e) == data[0]);
        (void)e;
    }
}

static string readFile(FILE *f) {
    string s;
    char buf[4096];
    for (int n; !feof(f) && (n = fread(buf, 1, sizeof(buf), f)) > 0; ) {
        s.insert(s.end(), buf, buf + n);
    }
    return s;
}

void tool_base64(int argc, char *argv[]) {
    --argc, ++argv;

    int isEncoding = true;
    if (argc > 0 && string(argv[0]) == "-d") {
        isEncoding = false;
        --argc, ++argv;
    }

    FILE *f = stdin;
    if (argc > 0) {
        if ((f = fopen(argv[0], "rb")) == nullptr) {
            fprintf(stderr, "Failed to open file: %s\n", argv[0]);
            return;
        }
    }

    string s = readFile(f);
    if (!s.empty()) {
        if (isEncoding) s = Base64().encode(s);
        else s = Base64().decode(s);
    }
    cout << s;

    if (argc > 0) fclose(f);
}
