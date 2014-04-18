
#include "pch.h"
#include "sha256.h"

static string str2Sha256(const string& s) {
    Sha256 m;
    if (!s.empty()) m.update(s.c_str(), (int)s.size());
    m.finalize();
    return m.digestStr();
}

void test_sha256() {
    string datas[][2] = {
        {"", "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855",},
        {"a", "ca978112ca1bbdcafac231b39a23dc4da786eff8147c4e72b9807785afee48bb",},
        {"aa", "961b6dd3ede3cb8ecbaacbd68de040cd78eb2ed5889130cceb4c49268ea4d506",},
        {"aaa", "9834876dcfb05cb167a5c24953eba58c4ac89b1adf57f28f2f9d09af107ee8f0",},
        {"5555555555555555555555555555555555555555555555555555555", "6ec1b49d80adafbe80682031f37aec4dc2ea60bc8c4be11103f08a57c6c9901c",},
        {"56565656565656565656565656565656565656565656565656565656", "519101bdac4fc1ff845626dbd367f8256c8c6d3cfe96a262b26d53a4c6823c65",},
        {"575757575757575757575757575757575757575757575757575757575", "cebb470d630caf52440c600633be85975c2c07bade36da82483215111f0b71af",},
        {"636363636363636363636363636363636363636363636363636363636363636", "f4afbb14c2bedf3128fa331b2ea46a417e0e51fa98370d532b86b73f8154f54a",},
        {"6464646464646464646464646464646464646464646464646464646464646464", "f572435c3fd11241828de21d1f590ee80ca6331356a2ae1f978ea655052737d7",},
        {"65656565656565656565656565656565656565656565656565656565656565656", "f47abc3f74e2628d9641b3382e1ac0c664416e8a47cb2b3421f94c58eef2469a",},
    };
    for (auto &data : datas) {
        assert(data[1] == str2Sha256(data[0]));
        (void)data;
        (void)str2Sha256;
    }
}

static string getFileSha256(FILE *f) {
    Sha256 m;
    char buf[16*1024];
    for (int n; !feof(f) && (n = (int)fread(buf, 1, sizeof(buf), f)) > 0; ) {
        m.update(buf, n);
    }
    m.finalize();
    return m.digestStr();
}

void tool_sha256(int argc, char *argv[]) {
    if (argc < 2) {
        cout << getFileSha256(stdin) << " -\n";
    } else {
        FILE *f;
        for (int i = 1; i < argc; ++i) {
            if ((f = fopen(argv[i], "rb")) == nullptr) {
                fprintf(stderr, "Fail to open file: %s\n", argv[i]);
                continue;
            }
            cout << getFileSha256(f) << " " << argv[i] << endl;
            fclose(f);
        }
    }
}
