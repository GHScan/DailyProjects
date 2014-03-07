#include "pch.h"

#include <unistd.h>

#include "Utils.h"
#include "IO.h"

int BlockingIO::readSome(int fd, char *buf, int size) {
    int n = 0;
    while ((n = ::read(fd, buf, size)) < 0) {
        P_ENSURE(errno == EINTR);
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
int BlockingIO::writeSome(int fd, const char *buf, int size) {
    int n;
    while ((n = ::write(fd, buf, size)) <= 0) {
        P_ENSURE(errno == EINTR);
    }
    return n;
}
void BlockingIO::writeN(int fd, const char *buf, int size) {
    while (size > 0) {
        int n = writeSome(fd, buf, size);
        ASSERT(n > 0);
        buf += n;
        size -= n;
    }
}

int NonblockingIO::readSome(int fd, char *buf, int size, bool &eof) {
    eof = false;

    int n = ::read(fd, buf, size);
    if (n < 0) {
        P_ENSURE(errno == EINTR || errno == EAGAIN);
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
        P_ENSURE(errno == EINTR || errno == EAGAIN);
        return 0;
    } else {
        return n;
    }
}
//////////////////////////////

BlockingReadBuffer::BlockingReadBuffer(int fd, int bufSize)
    : mBuf(bufSize), mDataBegin(nullptr), mDataEnd(nullptr), mFd(fd) {
}
bool BlockingReadBuffer::readLine(string &line) {
    line.clear();

    for (;;) {
        if (mDataBegin == mDataEnd) {
            if (!doReadFile()) return !line.empty();
        }
        ASSERT(mDataBegin < mDataEnd);

        char *p = mDataBegin;
        while (p != mDataEnd && p[0] != '\n') ++p;

        line.insert(line.end(), mDataBegin, p);

        if (p == mDataEnd) {
            mDataBegin = mDataEnd;
        } else {
            ASSERT(p[0] == '\n');
            mDataBegin = p + 1;
            return true;
        }
    }
    return false;
}
int BlockingReadBuffer::readN(char *buf, int size) {
    char *p = buf;
    while (size > 0) {
        if (mDataBegin == mDataEnd) {
            if (!doReadFile()) return p - buf;
        }
        ASSERT(mDataBegin < mDataEnd);

        int n = min(mDataEnd - mDataBegin, size);
        memcpy(buf, mDataBegin, n);
        buf += n;
        mDataBegin += n;
        size -= n;
    }
    return p - buf;
}
bool BlockingReadBuffer::doReadFile() {
    int n = BlockingIO::readN(mFd, &mBuf[0], mBuf.size());
    if (n == 0) return false;
    mDataBegin = &mBuf[0];
    mDataEnd = mDataBegin + n;
    return true;
}

BlockingWriteBuffer::BlockingWriteBuffer(int fd, int bufSize)
    : mBuf(bufSize), mFd(fd) {
    mSpaceBegin = &mBuf[0];
    mSpaceEnd = mSpaceBegin + bufSize;
}
void BlockingWriteBuffer::writeN(const char *buf, int size) {
    while (size > 0) {
        if (mSpaceBegin == mSpaceEnd) flush();
        ASSERT(mSpaceBegin < mSpaceEnd);

        int n = min(mSpaceEnd - mSpaceBegin, size);
        memcpy(mSpaceBegin, buf, n);
        mSpaceBegin += n;
        buf += n;
        size -= n;
    }
}
void BlockingWriteBuffer::flush() {
    BlockingIO::writeN(mFd, &mBuf[0], mSpaceBegin - &mBuf[0]);
    mSpaceBegin = &mBuf[0];
}
//////////////////////////////
NonblockingReadBufferManager::NonblockingReadBufferManager() {
}
NonblockingReadBufferManager::~NonblockingReadBufferManager() {
}
NonblockingReadBuffer* NonblockingReadBufferManager::createBuffer(int fd) {
    NonblockingReadBuffer *p = nullptr;
    if (!mFreeBufs.empty()) {
        p = mFreeBufs.back();
        mFreeBufs.pop_back();
    } else {
        p = new NonblockingReadBuffer();
    }
    p->init(fd);
    return p;
}
void NonblockingReadBufferManager::destroyBuffer(NonblockingReadBuffer *buf) {
    buf->uninit();
    mFreeBufs.push_back(buf);
}

bool NonblockingReadBuffer::readLine(string &line, bool &eof) {
    line.clear();

    eof = mEof && mDataEnd == 0;

    int linePos = -1;
    for (int i = mDataBegin; i < mDataEnd; ++i) {
        if (mBuf[i] == '\n') linePos = i;
    }

    int readBytes = 0;
    if (linePos != -1) {
        readBytes = linePos - mDataBegin;
    } else {
        if (mEof) {
            readBytes = mDataEnd - mDataBegin;
        }
    }

    line.resize(readBytes);
    readNFromBuf((char*)line.c_str(), readBytes);

    if (linePos != -1) discardBuf(1);

    return linePos != -1 || readBytes > 0;
}
int NonblockingReadBuffer::readN(char *buf, int size, bool &eof) {
    eof = mEof && mDataEnd == 0;

    int readBytes = 0;
    if (mDataEnd - mDataBegin >= size) {
        readBytes = size;
    } else {
        if (mEof) {
            readBytes = mDataEnd - mDataBegin;
        }
    }

    readNFromBuf(buf, readBytes);
    return readBytes;
}
void NonblockingReadBuffer::readNFromBuf(char *buf, int size) {
    ASSERT(mDataEnd - mDataBegin >= size);
    memcpy(buf, &mBuf[0] + mDataBegin, size);
    mDataBegin += size;
    if (mDataBegin == mDataEnd) mDataBegin = mDataEnd = 0;
}
void NonblockingReadBuffer::discardBuf(int size) {
    ASSERT(mDataEnd - mDataBegin >= size);
    mDataBegin += size;
    if (mDataBegin == mDataEnd) mDataBegin = mDataEnd = 0;
}
void NonblockingReadBuffer::notifyReadable() {
    for (;;) {
        if (mDataEnd == (int)mBuf.size()) mBuf.resize(mBuf.size() + 1024);

        int n = NonblockingIO::readSome(mFd, &mBuf[0] + mDataEnd, (int)mBuf.size() - mDataEnd, mEof);
        if (mEof) return;
        if (n == 0 && errno == EAGAIN) return;

        mDataEnd += n;
    }
}
void NonblockingReadBuffer::init(int fd) {
    mFd = fd;
    mDataBegin = mDataEnd = 0;
    mEof = false;
}
void NonblockingReadBuffer::uninit() {
    if (mDataEnd != mDataBegin) {
        LOG("Warning: %d bytes of data lost before reading ...", mDataEnd - mDataBegin);
    }
}
NonblockingReadBuffer::NonblockingReadBuffer(): mBuf(1024), mDataBegin(0), mDataEnd(0), mFd(-1), mEof(false) {
}
//////////////////////////////
NonblockingWriteBufferManager::NonblockingWriteBufferManager(): mBufCount(0) {
}
NonblockingWriteBufferManager::~NonblockingWriteBufferManager() {
    ASSERT(mBufCount == 0);
}
NonblockingWriteBuffer* NonblockingWriteBufferManager::createBuffer(int fd) {
    ++mBufCount;
    return new NonblockingWriteBuffer(this, fd);
}
void NonblockingWriteBufferManager::destroyBuffer(NonblockingWriteBuffer *buf) {
    --mBufCount;
    delete buf;
}
NonblockingWriteBufferManager::DataBlock* NonblockingWriteBufferManager::mallocDataBlock() {
    return (DataBlock*)mMemPool.malloc();
}
void NonblockingWriteBufferManager::freeDataBlock(DataBlock *b) {
    mMemPool.free(b);
}

void NonblockingWriteBuffer::write(const char *buf, int size) {
    while (size > 0) {
        NonblockingWriteBufferManager::DataBlock *b = mMgr->mallocDataBlock();
        int n = min((int)sizeof(b->data), size);
        memcpy(b->data, buf, n);
        buf += n;
        size -= n;

        b->payloadBegin = b->data;
        b->payloadEnd = b->data + n;
        b->next = mPendingDataBlocks;
        mPendingDataBlocks = b;
    }
}
void NonblockingWriteBuffer::notifyWriteable() {
    while (mPendingDataBlocks != nullptr) {
        NonblockingWriteBufferManager::DataBlock *b = mPendingDataBlocks;

        while (b->payloadBegin != b->payloadEnd) {
            int n = NonblockingIO::writeSome(mFd, b->payloadBegin, b->payloadEnd - b->payloadBegin);
            if (n == 0 && errno == EAGAIN) return;
            b->payloadBegin += n;
        }
        ASSERT(b->payloadBegin == b->payloadEnd);

        mPendingDataBlocks = b->next;
        mMgr->freeDataBlock(b);
    }
}
NonblockingWriteBuffer::NonblockingWriteBuffer(NonblockingWriteBufferManager *mgr, int fd)
    : mMgr(mgr), mPendingDataBlocks(nullptr), mFd(fd) {
}
NonblockingWriteBuffer::~NonblockingWriteBuffer() {
    if (mPendingDataBlocks != nullptr) {
        int bytes = 0;

        while (mPendingDataBlocks != nullptr) {
            NonblockingWriteBufferManager::DataBlock *b = mPendingDataBlocks;
            mPendingDataBlocks = b->next;

            bytes += b->payloadEnd - b->payloadBegin;
            mMgr->freeDataBlock(b);
        }

        LOG("Warning : %d bytes of data lost before writing...", bytes);
    }
}
