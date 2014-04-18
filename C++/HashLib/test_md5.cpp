
#include "pch.h"
#include "md5.h"

static string str2Md5(const string& s) {
    Md5 m;
    if (!s.empty()) m.update(s.c_str(), s.size());
    return m.hexdigest();
}

void test_md5() {
    string datas[][2] = {
        {"", "d41d8cd98f00b204e9800998ecf8427e",},
        {"a", "0cc175b9c0f1b6a831c399e269772661",},
        {"aa", "4124bc0a9335c27f086f24ba207a4912",},
        {"aaa", "47bce5c74f589f4867dbd57e9ca9f808",},
        {"5555555555555555555555555555555555555555555555555555555", "01bfd3f26aa0fe7e4d044e50eab21206",},
        {"56565656565656565656565656565656565656565656565656565656", "7a0228ae4501a408f05e1dd5445c093c",},
        {"575757575757575757575757575757575757575757575757575757575", "4f08a2d6be88c2ccc8b039d29d93e480",},
        {"636363636363636363636363636363636363636363636363636363636363636", "8483db53cf8a478cda41d70413a6ba01",},
        {"6464646464646464646464646464646464646464646464646464646464646464", "c1973c5d1d5c0cab6375a2c1684642cd",},
        {"65656565656565656565656565656565656565656565656565656565656565656", "5ea6f884c40d938d1fe6dce7a3c53fbf",},
    };
    for (auto &data : datas) {
        cout << data[0] << endl;
        cout << str2Md5(data[0]) << endl;
        assert(data[1] == str2Md5(data[0]));
    }
}

static string handleFile(FILE *f) {
    Md5 m;
    char buf[4096];
    for (int n; !feof(f) && (n = fread(buf, 1, sizeof(buf), f)) > 0; ) {
        m.update(buf, n);
    }
    return m.hexdigest();
}

void tool_md5(int argc, char *argv[]) {
    FILE *f = stdin;

    if (argc > 1) {
        if ((f = fopen(argv[1], "rb")) == nullptr) {
            fprintf(stderr, "Fail to open file: %s\n", argv[1]);
            return;
        }
    }

    cout << handleFile(f);

    if (argc > 1) fclose(f);
}
