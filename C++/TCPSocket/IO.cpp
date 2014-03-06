#include "pch.h"

#include <unistd.h>

#include "Utils.h"
#include "IO.h"

int BlockingIO::readSome(int fd, char *buf, int size) {
    int n = 0;
    while ((n = ::read(fd, buf, size)) < 0) {
        P_ASSERT(errno == EINTR);
    }
    return n;
}
int BlockingIO::readN(int fd, char *buf, int size) {
    char *p = buf;
    while (size > 0) {
        int n = readSome(fd, p, size);
        if (n == 0) return p - buf;
        ASSERT(n > 0);
        size -= n;
        p += n;
    }
    return p - buf;
}
int BlockingIO::writeN(int fd, const char *buf, int size) {
    const char *p = buf;
    while (size > 0) {
        int n = ::write(fd, p, size);
        if (n <= 0) {
            P_ASSERT(errno == EINTR);
            continue;
        } else {
            size -= n;
            p += n;
        }
    }
    return p - buf;
}

int NonblockingIO::readSome(int fd, char *buf, int size, bool &eof) {
    eof = false;

    int n = ::read(fd, buf, size);
    if (n < 0) {
        P_ASSERT(errno == EINTR || errno == EAGAIN);
        return 0;
    } else if (n == 0) {
        eof = true;
        return 0;
    } else {
        return n;
    }
}
int NonblockingIO::writeSome(int fd, const char *buf, int size) {
    int n = ::write(fd, buf, size);
    if  (n <= 0) {
        P_ASSERT(errno == EINTR || errno == EAGAIN);
        return 0;
    } else {
        return n;
    }
}
