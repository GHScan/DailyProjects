#ifndef TIMER_H
#define TIMER_H

#include <time.h>

#include <string>

class Timer {
public:
    Timer(const string& name = "", FILE *outFile = stdout): mName(name), mOutFile(outFile), mStart(clock()) {
    }
    ~Timer() {
        fprintf(mOutFile, "%s : %f s\n", mName.c_str(), float(clock() - mStart) / CLOCKS_PER_SEC);
    }
private:
    string mName;
    FILE *mOutFile;
    clock_t mStart;
};

#endif
