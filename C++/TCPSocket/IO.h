
#ifndef IO_H
#define IO_H

#include <utility>

#include "Utils.h"

//////////////////////////////
struct BlockingIO {
    static int readSome(int fd, char *buf, int size);
    static int readN(int fd, char *buf, int size);
    static int writeSome(int fd, const char *buf, int size);
    static void writeN(int fd, const char *buf, int size);
};
//------------------------------

struct NonblockingIO {
    static int readSome(int fd, char *buf, int size, bool &eof);
    static int writeSome(int fd, const char *buf, int size);
};
//////////////////////////////
class BlockingReadBuffer {
    DISABLE_COPY(BlockingReadBuffer);
public:
    BlockingReadBuffer(int fd, int bufSize);
    bool readLine(string &line);
    int readN(char *buf, int size);
private:
    bool doReadFile();
private:
    vector<char> mBuf;
    char *mDataBegin, *mDataEnd;
    int mFd;
};
//------------------------------

class BlockingWriteBuffer {
    DISABLE_COPY(BlockingWriteBuffer);
public:
    BlockingWriteBuffer(int fd, int bufSize);
    void writeN(const char *buf, int size);
    void flush();
private:
    vector<char> mBuf;
    char *mSpaceBegin, *mSpaceEnd;
    int mFd;
};

//////////////////////////////
class NonblockingReadBuffer;

class NonblockingReadBufferManager {
public:
    NonblockingReadBufferManager();
    ~NonblockingReadBufferManager();
    NonblockingReadBuffer* createBuffer(int fd);
    void destroyBuffer(NonblockingReadBuffer *buf);
private:
    vector<NonblockingReadBuffer*> mFreeBufs;
};

class NonblockingReadBuffer {
public:
    bool readLine(string &line, bool &eof);
    int readN(char *buf, int size, bool &eof);
    void notifyReadable();
private:
    void init(int fd);
    void uninit();
    NonblockingReadBuffer();
    void readNFromBuf(char *buf, int size);
    void discardBuf(int size);
    friend class NonblockingReadBufferManager;
private:
    vector<char> mBuf;
    int mDataBegin, mDataEnd;
    int mFd;
    bool mEof;
};

//------------------------------
class NonblockingWriteBuffer;

class NonblockingWriteBufferManager {
public:
    NonblockingWriteBufferManager();
    ~NonblockingWriteBufferManager();
    NonblockingWriteBuffer* createBuffer(int fd);
    void destroyBuffer(NonblockingWriteBuffer *buf);
private:
    struct DataBlock {
        DataBlock *next;
        char *payloadBegin, *payloadEnd;
        char data[4 * 1024 - sizeof(DataBlock*) - sizeof(char*) * 2];
    };
    friend class NonblockingWriteBuffer;
private:
    DataBlock* mallocDataBlock();
    void freeDataBlock(DataBlock *b);
private:
    MemoryPool<sizeof(DataBlock), 32 * (4 * 1024)> mMemPool;
    int mBufCount;
};

class NonblockingWriteBuffer {
public:
    void write(const char *buf, int size);
    void notifyWriteable();
    bool hasPendingData() const { return mPendingDataBlocks != nullptr; }
private:
    NonblockingWriteBuffer(NonblockingWriteBufferManager *mgr, int fd);
    ~NonblockingWriteBuffer();
    friend class NonblockingWriteBufferManager;
private:
    NonblockingWriteBufferManager *mMgr;
    NonblockingWriteBufferManager::DataBlock *mPendingDataBlocks;
    int mFd;
};

#endif
