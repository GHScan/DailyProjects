#include "pch.h"
#include "stream.h"

StringInputStream::StringInputStream(const string &s): mStr(s), mOff(0) {
}

int StringInputStream::read(void *buf, int n) {
    if (mOff == (int)mStr.size()) return 0;
    int bytes = min((int)mStr.size() - mOff, n);
    memcpy(buf, mStr.c_str() + mOff, bytes);
    mOff += bytes;
    return bytes;
}

int StringInputStream::size() {
    return (int)mStr.size();
}

StringOutputStream::StringOutputStream(string &s): mStr(s) {
}

int StringOutputStream::write(const void *_buf, int n) {
    auto buf = (const char *)_buf;
    mStr.insert(mStr.end(), buf, buf + n);
    return n;
}

FileInputStream::FileInputStream(FILE *f): mF(f) {
}

int FileInputStream::read(void *buf, int n) {
    return fread(buf, 1, n, mF);
}

int FileInputStream::size() {
    long cur = ftell(mF);
    fseek(mF, 0, SEEK_END);
    long size = ftell(mF);
    fseek(mF, cur, SEEK_SET);
    return size;
}

FileOutputStream::FileOutputStream(FILE *f): mF(f) {
}

int FileOutputStream::write(const void *buf, int n) {  
    return fwrite(buf, 1, n, mF);
}
