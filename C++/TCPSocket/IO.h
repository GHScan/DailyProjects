
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

// It is a convenience class for read blocking-socket and file. For file, it
// can reduce the sys-call time, by batch the read operation with bufsize
// aligned; For socket, it make the read snippet been transparent for user
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

// It is designed to work with blocking-socket and file; For file, it reduce
// the sys-call time by batch the write operation with bufsize aligned
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
class EventDrivenReadBuffer;

class EventDrivenReadBufferManager {
public:
    EventDrivenReadBufferManager();
    ~EventDrivenReadBufferManager();
    EventDrivenReadBuffer* createBuffer(int fd);
    void destroyBuffer(EventDrivenReadBuffer *buf);
private:
    vector<EventDrivenReadBuffer*> mFreeBufs;
};

class EventDrivenReadBuffer {
public:
    bool readLine(string &line, bool &eof);
    int readN(char *buf, int size, bool &eof);
    void onReadNonblockingFd();
    void onReadBlockingFd();
private:
    void init(int fd);
    void uninit();
    EventDrivenReadBuffer();
    void readNFromBuf(char *buf, int size);
    void discardBuf(int size);
    friend class EventDrivenReadBufferManager;
private:
    vector<char> mBuf;
    int mDataBegin, mDataEnd;
    int mFd;
    bool mEof;
};

//------------------------------
class EventDrivenWriteBuffer;

class EventDrivenWriteBufferManager {
public:
    EventDrivenWriteBufferManager();
    ~EventDrivenWriteBufferManager();
    EventDrivenWriteBuffer* createBuffer(int fd);
    void destroyBuffer(EventDrivenWriteBuffer *buf);
private:
    struct DataBlock {
        DataBlock *next;
        char *payloadBegin, *payloadEnd;
        char data[4 * 1024 - sizeof(DataBlock*) - sizeof(char*) * 2];
    };
    friend class EventDrivenWriteBuffer;
private:
    DataBlock* mallocDataBlock();
    void freeDataBlock(DataBlock *b);
private:
    MemoryPool<sizeof(DataBlock), 32 * (4 * 1024)> mMemPool;
    int mBufCount;
};

class EventDrivenWriteBuffer {
public:
    void write(const char *buf, int size);
    void onWriteNonblockingFd();
    void onWriteBlockingFd();
    bool hasPendingData() const { return mPendingDataBlocks != nullptr; }
private:
    EventDrivenWriteBuffer(EventDrivenWriteBufferManager *mgr, int fd);
    ~EventDrivenWriteBuffer();
    friend class EventDrivenWriteBufferManager;
private:
    EventDrivenWriteBufferManager *mMgr;
    EventDrivenWriteBufferManager::DataBlock *mPendingDataBlocks;
    int mFd;
};

#endif
