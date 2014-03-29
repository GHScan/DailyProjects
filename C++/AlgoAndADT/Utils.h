#ifndef UTILS_H
#define UTILS_H

#include <time.h>

#include <string>
#include <set>
#include <sstream>

#include <sys/time.h>
#include <pthread.h>

static inline double getTime() {
    timeval tv;
    gettimeofday(&tv, nullptr);
    return tv.tv_sec + tv.tv_usec / 1000000.0;
}

class Timer {
public:
    Timer(const string& name = "", FILE *outFile = stdout): mName(name), mOutFile(outFile), mStart(getTime()) {
    }
    ~Timer() {
        fprintf(mOutFile, "%s : %f s\n", mName.c_str(), getTime() - mStart);
    }
private:
    string mName;
    FILE *mOutFile;
    double mStart;
};

template<typename T>
static inline void assertOrdered(const T *begin, const T *end) {
    for (; begin < end - 1; ++begin) {
        assert(begin[0] <= begin[1]);
    }
}

static inline int myrand(int begin, int end) {
    uint32_t r = rand() * RAND_MAX + rand();
    r = r % uint32_t(end - begin) + begin;
    return int(r);
}

static inline void setCpuAffinity(int mask) {
    cpu_set_t cpuset;
    CPU_ZERO(&cpuset);
    for (int i = 0; i < 32; ++i) {
        if ((mask >> i) & 1) {
            CPU_SET(i, &cpuset);
        }
    }

    pthread_setaffinity_np(pthread_self(), sizeof(cpuset), &cpuset);
}

static inline string checkoutCmd(const char *cmd) {
    string ret;

    string tfname = "__113adf1.txt";
    int err = system(("(" + string(cmd) + ")>" + tfname).c_str());
    if (err == -1) return ret;

    FILE *f = fopen(tfname.c_str(), "rb");
    if (f != nullptr) {
        fseek(f, 0, SEEK_END);
        int len = ftell(f);
        fseek(f, 0, SEEK_SET);
        ret.resize(len);
        if (fread((char*)ret.c_str(), len, 1, f) != 1) {
            fprintf(stderr, "read failed!");
        }
        fclose(f);
    }

    err = remove(tfname.c_str());
    if (err == -1) return "";

    return ret;
}

static inline vector<string> splitToKeywords(const string &_str, const string &delim = " \t\n") {
    vector<char> str(_str.c_str(), _str.c_str() + _str.size());

    set<string> out;
    for (char *p = &str[0]; ; p = nullptr) {
        char *tok = strtok(p, delim.c_str());
        if (tok == nullptr) break;
        out.insert(tok);
    }

    return vector<string>(out.begin(), out.end());
}

template<typename T>
static inline string toString(const T &v) {
    ostringstream so;
    so << v;
    return so.str();
}

#endif
