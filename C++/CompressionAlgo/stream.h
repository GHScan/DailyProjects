#ifndef STREAM_H
#define STREAM_H

#include <assert.h>

struct IInputStream {
    virtual ~IInputStream(){}
    virtual int read(void *buf, int n) = 0;
    virtual int size() = 0;

    template<typename T>
    T read() {
        T v;
        if (read(&v, sizeof(v)) != sizeof(v)) assert(0);
        return v;
    }
};
struct IOutputStream {
    virtual ~IOutputStream() {}
    virtual int write(const void *buf, int n) = 0;

    template<typename T>
    void write(const T &val) {
        if (write(&val, sizeof(val)) != sizeof(val)) assert(0);
    }
};

class StringInputStream: public IInputStream {
public:
    StringInputStream(const string &s);
    virtual int read(void *buf, int n);
    virtual int size();
private:
    const string &mStr;
    int mOff;
};
class StringOutputStream: public IOutputStream {
public:
    StringOutputStream(string &s);
    virtual int write(const void *buf, int n);
private:
    string &mStr;
};

class FileInputStream: public IInputStream {
public:
    FileInputStream(FILE *f);
    virtual int read(void *buf, int n);
    virtual int size();
private:
    FILE *mF;
};
class FileOutputStream: public IOutputStream {
public:
    FileOutputStream(FILE *f);
    virtual int write(const void *buf, int n);
private:
    FILE *mF;
};

#endif
