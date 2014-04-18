
#include "pch.h"
#include "sha1.h"

static string str2Sha1(const string& s) {
    Sha1 m;
    if (!s.empty()) m.update(s.c_str(), (int)s.size());
    m.finalize();
    return m.digestStr();
}

void test_sha1() {
    string datas[][2] = {
        {"", "da39a3ee5e6b4b0d3255bfef95601890afd80709",},
        {"a", "86f7e437faa5a7fce15d1ddcb9eaeaea377667b8",},
        {"aa", "e0c9035898dd52fc65c41454cec9c4d2611bfb37",},
        {"aaa", "7e240de74fb1ed08fa08d38063f6a6a91462a815",},
        {"5555555555555555555555555555555555555555555555555555555", "595a6510b007e4db44ded36cf2ff627626f9cac2",},
        {"56565656565656565656565656565656565656565656565656565656", "b46e01cadc5a50c554a403220d3a964010224176",},
        {"575757575757575757575757575757575757575757575757575757575", "8a24377d85eecd4986808655b40cbf6dbedfc7f6",},
        {"636363636363636363636363636363636363636363636363636363636363636", "a52463ab85dac9a889d56d96ab8ac62d5fc925e7",},
        {"6464646464646464646464646464646464646464646464646464646464646464", "4b3557f9f18d771f1def9bf8927cdebcf3208e4f",},
        {"65656565656565656565656565656565656565656565656565656565656565656", "65d37a01d759de789011c3db6420be4cc6348ef4",},
    };
    for (auto &data : datas) {
        assert(data[1] == str2Sha1(data[0]));
        (void)data;
        (void)str2Sha1;
    }
}

static string getFileSha1(FILE *f) {
    Sha1 m;
    char buf[16*1024];
    for (int n; !feof(f) && (n = (int)fread(buf, 1, sizeof(buf), f)) > 0; ) {
        m.update(buf, n);
    }
    m.finalize();
    return m.digestStr();
}

void tool_sha1(int argc, char *argv[]) {
    if (argc < 2) {
        cout << getFileSha1(stdin) << " -\n";
    } else {
        FILE *f;
        for (int i = 1; i < argc; ++i) {
            if ((f = fopen(argv[i], "rb")) == nullptr) {
                fprintf(stderr, "Fail to open file: %s\n", argv[i]);
                continue;
            }
            cout << getFileSha1(f) << " " << argv[i] << endl;
            fclose(f);
        }
    }
}
