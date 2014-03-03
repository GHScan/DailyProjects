
#ifndef TIME_H
#define TIME_H

#include <sys/time.h>

class Timer {
public:
    Timer(FILE *f, const char *name): mFile(f), mName(name), mStart(getTime()) { }
    ~Timer() {
        fprintf(mFile, "%s: %.3fsec\n", mName, getTime() - mStart);
    }
    static double getTime() {
        timeval tv;
        gettimeofday(&tv, NULL);
        return tv.tv_sec + double(tv.tv_usec) / 1000000;
    }
private:
    FILE *mFile;
    const char *mName;
    double mStart;
};

#endif
