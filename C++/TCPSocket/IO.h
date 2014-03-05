
#ifndef IO_H
#define IO_H

#include <utility>

struct BlockingIO {
    static int readSome(int fd, char *buf, int size);
    static int readN(int fd, char *buf, int size);
    static int writeN(int fd, const char *buf, int size);
};

struct NonblockingIO {
    static int readSome(int fd, char *buf, int size, bool &eof);
    static int writeSome(int fd, const char *buf, int size);
};

#endif
