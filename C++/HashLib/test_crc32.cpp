
#include "pch.h"
#include "crc32.h"

void test_crc32() {
    {
        Crc32_littleEndian c;
        auto crc = c.update("ab", 2);
        crc = c.update("c", 1, crc);

        assert(c.update("abc", 3) == crc);
    }

    {
        Crc32 c;

        string datas[][2] = {
            {"a", "-390611389",},
            {"aa", "126491095",},
            {"aaa", "-267947219",},
            {"5555555555555555555555555555555555555555555555555555555", "-37210464",},
            {"56565656565656565656565656565656565656565656565656565656", "-1544667232",},
            {"575757575757575757575757575757575757575757575757575757575", "791018464",},
            {"636363636363636363636363636363636363636363636363636363636363636", "635346164",},
            {"6464646464646464646464646464646464646464646464646464646464646464", "1141268591",},
            {"65656565656565656565656565656565656565656565656565656565656565656", "69915101",},
        };
        for (auto &data : datas) {
            assert((int)c.update(data[0].c_str(), data[0].size()) == atoi(data[1].c_str()));

            if (data[0].size() > 1) {
                auto crc = c.update(data[0].c_str(), data[0].size() / 2);
                crc = c.update(data[0].c_str() + data[0].size() / 2, data[0].size() - data[0].size() / 2, crc);
                assert(c.update(data[0].c_str(), data[0].size()) == crc);
            }
        }
    }
}

static uint32_t getFileCrc(FILE *f, Crc32 &c) {
    uint32_t crc = Crc32::INIT_CRC;

    char buf[16*1024];
    for (int n; !feof(f) && (n = (int)fread(buf, 1, sizeof(buf), f)) > 0; ) {
        crc = c.update(buf, n, crc);
    }

    return crc;
}

void tool_crc32(int argc, char *argv[]) {
    Crc32 c;

    if (argc < 2) {
        printf("%08x -\n", getFileCrc(stdin, c));
    } else {
        FILE *f;
        for (int i = 1; i < argc; ++i) {
            if ((f = fopen(argv[i], "rb")) == nullptr) {
                fprintf(stderr, "Fail to open file: %s\n", argv[i]);
                continue;
            }
            printf("%08x %s\n", getFileCrc(f, c), argv[i]);
            fclose(f);
        }
    }
}
