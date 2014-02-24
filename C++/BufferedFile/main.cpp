#include "pch.h"

#include <stdarg.h>

#include <unistd.h>
#include <fcntl.h>
#include <sys/time.h>
#include <sys/mman.h>

//////////////////////////////
static const char* format(const char *fmt, ...) {
    static char sBuf[512];
    va_list args;
    va_start(args, fmt);
    vsprintf(sBuf, fmt, args);
    va_end(args);
    return sBuf;
}

static double getTime() {
    timeval tv;
    gettimeofday(&tv, NULL);
    return tv.tv_sec + double(tv.tv_usec) / 1000000;
}

#define timeit(cmds) { double start = getTime(); cmds; printf("timeit line=%d : %.3f\n", __LINE__, getTime() - start); }
//////////////////////////////
struct IFile {
    enum Anchor {
        A_Begin,
        A_Cur,
        A_End,
    };
    virtual ~IFile() {}
    virtual int open(const char *path, const char *mode) = 0;
    virtual int read(void *buf, int len) = 0;
    virtual int write(const void *buf, int len) = 0;
    virtual int tell() = 0;
    virtual int seek(Anchor a, int off) = 0;
    virtual void close() = 0;
};

class OSFile: public IFile {
public:
    OSFile(const char *path, const char *mode): mFd(-1) { 
        open(path, mode);
    }
    OSFile(int fd): mFd(fd) {
    }
    ~OSFile() { 
        close();
    }
    int detach() { 
        int fd = mFd;
        mFd = -1;
        return fd;
    }
    virtual int open(const char *path, const char *mode) {
        assert(mFd == -1);

        int flag = O_RDONLY;
        for (; *mode; ++mode) {
            switch (*mode) {
                case 'r': flag |= O_RDONLY; break;
                case 'w': flag |= O_WRONLY | O_CREAT; break;
                case 'a': flag = (flag | O_APPEND) & ~O_CREAT; break;
                case '+': flag |= O_RDWR; break;
                default: break;
            }
        }
        mFd = ::open(path, flag, S_IRUSR | S_IWUSR);
        return mFd;
    }
    virtual int read(void *buf, int len) {
        return ::read(mFd, buf, len);
    }
    virtual int write(const void *buf, int len) {
        return ::write(mFd, buf, len);
    }
    virtual int tell() {
        return seek(A_Cur, 0);
    }
    virtual int seek(Anchor a, int off) {
        int where;
        switch (a) {
            case A_Begin: where = SEEK_SET; break;
            case A_Cur: where = SEEK_CUR; break;
            case A_End: where = SEEK_END; break;
            default: where = SEEK_SET; assert(0); break;
        }
        return ::lseek(mFd, off, where);
    }
    virtual void close() {
        if (mFd != -1) {
            ::close(mFd);
            mFd = -1;
        }
    }
private:
    OSFile(const OSFile&);
    OSFile& operator = (const OSFile&);
private:
    int mFd;
};

class BufferedFile: public IFile {
public:
    BufferedFile(IFile *file, bool isInputBuf = true, int bufLen = 4096): 
        mFile(file), mBufLen(bufLen), mBufData(NULL), mBufDataLen(0), mIsInputBuf(isInputBuf) {
        mBuf = new char[mBufLen];
    }
    ~BufferedFile() {
        close();
        delete[] mBuf;
        delete mFile;
    }
    virtual int open(const char *path, const char *mode) { 
        return mFile->open(path, mode);
    }
    virtual void close() {
        if (!mIsInputBuf) flush();
        mFile->close();
    }
    virtual int tell() { 
        return seek(A_Cur, 0); 
    }
    virtual int seek(Anchor a, int off) {
        if (mIsInputBuf) {
            return mFile->seek(a, off) - mBufDataLen;
        } else {
            return mFile->seek(a, off) + mBufDataLen;
        }
    }
    virtual int write(const void *buf, int len) {
        assert(!mIsInputBuf);

        const char *_buf = (const char*)buf;
        int leftLen = len;

        while (leftLen > 0) {
            if (mBufDataLen == mBufLen) {
                flush();
                mBufDataLen = 0;
            }

            int n = min(leftLen, mBufLen - mBufDataLen);
            memcpy(mBuf + mBufDataLen, _buf, n);
            _buf += n;
            leftLen -= n;
            mBufDataLen += n;
        }

        return len;
    }
    virtual int read(void *buf, int len) {
        assert(mIsInputBuf);

        char *_buf = (char*)buf;
        int leftLen = len;

        while (leftLen > 0) {
            if (mBufDataLen == 0) {
                mBufDataLen = mFile->read(mBuf, mBufLen);
                if (mBufDataLen == 0) return len - leftLen;
                mBufData = mBuf;
            }

            int n = min(mBufDataLen, leftLen);
            memcpy(_buf, mBufData, n);
            _buf += n;
            leftLen -= n;
            mBufData += n;
            mBufDataLen -= n;
        }

        return len;
    }
    void flush() {
        assert(!mIsInputBuf);

        mBufData = mBuf;
        while (mBufDataLen > 0) {
            int n = mFile->write(mBufData, mBufDataLen);
            mBufData += n;
            mBufDataLen -= n;
        }
    }
private:
    BufferedFile(const BufferedFile&);
    BufferedFile& operator = (const BufferedFile&);
private:
    IFile *mFile;
    char *mBuf;
    int mBufLen;
    char *mBufData;
    int mBufDataLen;
    bool mIsInputBuf;
};

//////////////////////////////
static int createEmptyFile(const char *path, int pageCount) {
    return ::system(format("dd if=/dev/zero of=%s bs=4K count=%d", path, pageCount));
}
static int rmEmptyFile(const char *path) {
    return ::system(format("rm %s", path));
}
static void checkSameFile(const char *path1, const char *path2) {
    FILE *f1 = fopen(path1, "r");
    FILE *f2 = fopen(path2, "r");
    char buf1[4096], buf2[4096];
    for (;;) {
        int n1 = fread(buf1, 1, sizeof buf1, f1);
        int n2 = fread(buf2, 1, sizeof buf2, f2);
        assert(n1 == n2);
        assert(memcmp(buf1, buf2, n1) == 0);
        if (n1 == 0) break;
    }
    fclose(f1);
    fclose(f2);
}

static void copyFile_mmap(int fdi, int fdo, int bufLen) {
    if (fdi == -1 || fdo == -1) perror("copyFile_mmap, invalid file:");

    int len = ::lseek(fdi, 0, SEEK_END);
    ::lseek(fdi, 0, SEEK_SET);
    if (len <= 0) perror("copyFile_mmap, invalid len:");
    if (::ftruncate(fdo, len) == -1) perror("copyFile_mmap, truncate failed:");

    int off = 0;
    while (len > 0) {
        int n = min(len, bufLen);

        void *psrc = mmap(NULL, n, PROT_READ, MAP_SHARED, fdi, off);
        void *pdest = mmap(NULL, n, PROT_WRITE, MAP_SHARED, fdo, off);
        if (pdest == (void*)-1) perror("copyFile_mmap, mmap failed dest:");
        if (psrc == (void*)-1) perror("copyFile_mmap, mmap failed src:");

        memcpy(pdest, psrc, n);

        munmap(psrc, n);
        munmap(pdest, n);

        off += n;
        len -= n;
    }
}
static void copyFile_IFile(IFile *fi, IFile *fo, vector<char>& buf) {
    while (int n = fi->read(&buf[0], (int)buf.size())) {
        fo->write(&buf[0], n);
    }
}
static void copyFile_ANSICfgetc(FILE *fi, FILE *fo) {
    int c;
    while ((c = fgetc(fi)) != EOF) {
        fputc(c, fo);
    }
}
static void copyFile_ANSICfread(FILE *fi, FILE *fo, vector<char>& buf) {
    while (int n = fread(&buf[0], 1, buf.size(), fi)) {
        fwrite(&buf[0], 1, n, fo);
    }
}

int main(int argc, char* argv[]) {
    int pageCount = argc > 1 ? atoi(argv[1]) : 1234;

    const int COPY_TIME = 10;
    const int MIN_BUF_LEN = 1 << 7;
    const int MAX_BUF_LEN = 1 << 14;

    {
        createEmptyFile("1.txt", pageCount);
        FILE *fi = fopen("1.txt", "rb");
        FILE *fo = fopen("2.txt", "wb");
        puts("copyFile_ANSICfgetc:");
        timeit(
                for (int i = 0; i < COPY_TIME; ++i) {
                fseek(fi, 0, SEEK_SET);
                fseek(fo, 0, SEEK_SET);
                copyFile_ANSICfgetc(fi, fo);
                }
              );
        fclose(fi);
        fclose(fo);
        checkSameFile("1.txt", "2.txt");
        rmEmptyFile("1.txt");
        rmEmptyFile("2.txt");
    }
    puts("finish!\n\n");

    for (int i = 0; i < 4; ++i)
    {
        createEmptyFile("1.txt", pageCount);
        int fdi = ::open("1.txt", O_RDONLY);
        int fdo = ::open("2.txt", O_RDWR | O_CREAT, S_IRUSR |S_IWUSR);
        const int BUFF_LENS[] = {1 << 12, 1 << 13, 1 << 20, 1 << 21};
        const int bufLen = BUFF_LENS[i];
        printf("copyFile_mmap: %.3fK\n", bufLen / float(1024));
        timeit(
                for (int i = 0; i < COPY_TIME; ++i) {
                ::lseek(fdi, 0, SEEK_SET);
                ::lseek(fdo, 0, SEEK_SET);
                copyFile_mmap(fdi, fdo, bufLen);
                }
              );
        ::close(fdi);
        ::close(fdo);
        checkSameFile("1.txt", "2.txt");
        rmEmptyFile("1.txt");
        rmEmptyFile("2.txt");
    }
    puts("finish!\n\n");

    for (int i = MIN_BUF_LEN; i <= MAX_BUF_LEN; i <<= 1)
    {
        createEmptyFile("1.txt", pageCount);
        FILE *fi = fopen("1.txt", "rb");
        FILE *fo = fopen("2.txt", "wb");
        vector<char> buf(i);
        printf("copyFile_ANSICfread: %.3fK\n", float(buf.size()) / 1024);
        timeit(
                for (int i = 0; i < COPY_TIME; ++i) {
                fseek(fi, 0, SEEK_SET);
                fseek(fo, 0, SEEK_SET);
                copyFile_ANSICfread(fi, fo, buf);
                }
              );
        fclose(fi);
        fclose(fo);
        checkSameFile("1.txt", "2.txt");
        rmEmptyFile("1.txt");
        rmEmptyFile("2.txt");
    }
    puts("finish!\n\n");

    for (int i = MIN_BUF_LEN; i <= MAX_BUF_LEN; i <<= 1)
    {
        createEmptyFile("1.txt", pageCount);
        {
            OSFile fi("1.txt", "r"), fo("2.txt", "w");
            vector<char> buf(i);
            printf("copyFile_OSFile :%.3fK\n", float(buf.size()) / 1024);
            timeit(
                    for (int i = 0; i < COPY_TIME; ++i) {
                    fi.seek(IFile::A_Begin, 0);
                    fo.seek(IFile::A_Begin, 0);
                    copyFile_IFile(&fi, &fo, buf);
                    }
                  );
        }
        checkSameFile("1.txt", "2.txt");
        rmEmptyFile("1.txt");
        rmEmptyFile("2.txt");
    }
    puts("finish!\n\n");

    for (int i = MIN_BUF_LEN; i <= MAX_BUF_LEN; i <<= 1)
    {
        createEmptyFile("1.txt", pageCount);
        {
            BufferedFile fi(new OSFile("1.txt", "r"));
            BufferedFile fo(new OSFile("2.txt", "w"), false);
            vector<char> buf(i);
            printf("copyFile_BufferedFile :%.3fK\n", float(buf.size()) / 1024);
            timeit(
                    for (int i = 0; i < COPY_TIME; ++i) {
                    fi.seek(IFile::A_Begin, 0);
                    fo.seek(IFile::A_Begin, 0);
                    copyFile_IFile(&fi, &fo, buf);
                    fo.flush();
                    }
                  );
        }
        checkSameFile("1.txt", "2.txt");
        rmEmptyFile("1.txt");
        rmEmptyFile("2.txt");
    }
    puts("finish!\n\n");
}
