#include "pch.h"

#include <map>

static string method_divideConquer(const string &line) {
    if (line.size() <= 1) return line;
    if (line.front() == line.back()) return line.front() + method_divideConquer(line.substr(1, line.size() - 2)) + line.front();
    string s1 = line.back() + method_divideConquer(line.substr(0, line.size() - 1)) + line.back();
    string s2 = line.front() + method_divideConquer(line.substr(1, line.size() - 1)) + line.front();
    return s1.size() < s2.size() ? s1 : s2;
}

static string _method_memoize(const string &line, map<string, string> &mem) {
    if (mem.count(line) > 0) return mem[line];

    string out;

    if (line.size() <= 1) out = line;
    else if (line.front() == line.back()) out = line.front() + _method_memoize(line.substr(1, line.size() - 2), mem) + line.front();
    else {
        string s1 = line.back() + _method_memoize(line.substr(0, line.size() - 1), mem) + line.back();
        string s2 = line.front() + _method_memoize(line.substr(1, line.size() - 1), mem) + line.front();
        out = s1.size() < s2.size() ? s1 : s2;
    }

    mem[line] = out;
    return out;
}
static string method_memoize(const string &line) {
    map<string, string> mem;
    string out = _method_memoize(line, mem);
    return out;
}

static string method_dp(const string &line) {
    if (line.size() < 3) return method_divideConquer(line);

    vector<string> v3[3];
    v3[0].resize(line.size());
    v3[1].resize(line.size());
    v3[2].resize(line.size());

    for (int i = 0; i < (int)line.size(); ++i) v3[0][i] = line.substr(i, 1);

    int len = 2, idx = 1;
    for (; len <= (int)line.size(); idx = (idx + 1) % 3, ++len) {
        int prevIdx1 = (idx + 2) % 3;
        int prevIdx2 = (idx + 1) % 3;
        for (int start = 0; start <= (int)line.size() - len; ++start) {
            char front = line[start], back = line[start + len - 1];
            if (front == back) {
                v3[idx][start] = front + v3[prevIdx2][start + 1] + front;
            } else {
                const string &s1 = v3[prevIdx1][start];
                const string &s2 = v3[prevIdx1][start + 1];
                v3[idx][start] = s1.size() < s2.size() ? back + s1 + back: front + s2 + front;
            }
        }
    }

    return v3[(idx + 2) % 3][0];
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        printf("Usage: %s 1/2/3", argv[0]);
        return 1;
    }

    for (string line; getline(cin, line); ) {
        string out;
        switch (atoi(argv[1])) {
            case 0: out = method_divideConquer(line); break;
            case 1: out = method_memoize(line); break;
            case 2: out = method_dp(line); break;
            default: break;
        }
        // cout << line << " ->\n" << out << endl;
    }
}
