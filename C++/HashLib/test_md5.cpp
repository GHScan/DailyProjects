
#include "pch.h"
#include "md5.h"

static string str2Md5(const string& s) {
    Md5 m;
    if (!s.empty()) m.update(s.c_str(), (int)s.size());
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
        assert(data[1] == str2Md5(data[0]));
        (void)data;
        (void)str2Md5;
    }
}

static string getFileMd5(FILE *f) {
    Md5 m;
    char buf[4096];
    for (int n; !feof(f) && (n = (int)fread(buf, 1, sizeof(buf), f)) > 0; ) {
        m.update(buf, n);
    }
    return m.hexdigest();
}

void tool_md5(int argc, char *argv[]) {
    if (argc < 2) {
        cout << getFileMd5(stdin) << " -\n";
    } else {
        FILE *f;
        for (int i = 1; i < argc; ++i) {
            if ((f = fopen(argv[i], "rb")) == nullptr) {
                fprintf(stderr, "Fail to open file: %s\n", argv[i]);
                continue;
            }
            cout << getFileMd5(f) << " " << argv[i] << endl;
            fclose(f);
        }
    }
}
