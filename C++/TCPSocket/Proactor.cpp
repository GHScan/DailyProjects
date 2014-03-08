#include "pch.h"

#include <unistd.h>

#include "Proactor.h"

ProactorService::ProactorService(const char *pollerType, bool blocking): 
    mPoller(IPoller::create(pollerType)), mBlocking(blocking) {
}
ProactorService::~ProactorService() {
    while (!mFreeFiles.empty()) {
        delete mFreeFiles.back();
        mFreeFiles.pop_back();
    }

    DELETE(mPoller);
}
void ProactorService::createClientSocket(const HostAddress &serverAddr, function<void(ProactorFile* file)> connectedCallback) {
    TCPSocket socket = TCPSocket::create();
    socket.setNonBlocking(true);
    if (socket.connectAsync(serverAddr)) {
        socket.setNonBlocking(!mBlocking);

        ProactorFile *file = allocFile();
        file->init(this, socket.getFd());
        connectedCallback(file);
    } else {
        ProactorFile *file = allocFile();
        file->initWithConnectedCallbak(this, socket.getFd(), connectedCallback);
    }
}
ProactorFile* ProactorService::createListenSocket(const HostAddress &bindAddr, int backLog) {
    TCPSocket socket = TCPSocket::create();
    socket.bind(bindAddr);
    socket.listen(backLog);
    socket.setNonBlocking(!mBlocking);

    ProactorFile *file = allocFile();
    file->init(this, socket.getFd());
    return file;
}
ProactorFile* ProactorService::attachFd(int fd) {
    TCPSocket::fromFd(fd).setNonBlocking(!mBlocking);

    ProactorFile *file = allocFile();
    file->init(this, fd);
    return file;
}
void ProactorService::destroyFile(ProactorFile *file) {
    file->uninit();
    freeFile(file);
}
void ProactorService::wait(int timeout) {
    vector<IPoller::Event> events;
    if (!mPoller->wait(events, timeout)) return;

    for (auto &event : events) {
        if (event.flag & IPoller::EF_Readable) ((ProactorFile*)event.ud)->onRead();
        if (event.flag & IPoller::EF_Writeable) ((ProactorFile*)event.ud)->onWrite();
        if (event.flag & IPoller::EF_ErrFound) {
            auto f = (ProactorFile*)event.ud;
            LOG_ERR("Found error ini proactor file: %d,%s", f->getFd(), strerror(errno));
            destroyFile(f);
        }
    }
}
ProactorFile* ProactorService::allocFile() {
    if (!mFreeFiles.empty()) {
        ProactorFile *file = mFreeFiles.back();
        mFreeFiles.pop_back();
        return file;
    }
    return new ProactorFile();
}
void ProactorService::freeFile(ProactorFile *file) {
    mFreeFiles.push_back(file);
}

void ProactorFile::accept(function<void(ProactorFile* file, const HostAddress& addr)> callback) {
    ASSERT(mAcceptCallback == nullptr);
    mAcceptCallback = callback;
}
void ProactorFile::readLine(char delmit, function<void(bool eof, const char *buf, int n)> callback) {
    mReadBuf.readLine(delmit, callback);
}
void ProactorFile::readN(int n, function<void(bool eof, const char *buf, int n)> callback) {
    mReadBuf.readN(n, callback);
}
void ProactorFile::writeN(const char *buf, int n, function<void()> callback) {
    if (!mWriteBuf.hasPendingData()) {
        mService->getPoller()->update(mFd, this, IPoller::EF_Readable | IPoller::EF_Writeable);
    }
    mWriteBuf.writeN(buf, n, callback);
}
void ProactorFile::init(ProactorService *service, int fd) {
    mReadBuf.init(fd);
    mWriteBuf.init(fd);
    mFd = fd;
    mService = service;
    mService->getPoller()->add(mFd, this, IPoller::EF_Readable);
}
void ProactorFile::initWithConnectedCallbak(ProactorService *service, int fd, function<void(ProactorFile*)> callback) {
    mConnectedCallback = callback;
    mReadBuf.init(fd);
    mWriteBuf.init(fd);
    mFd = fd;
    mService = service;
    mService->getPoller()->add(mFd, this, IPoller::EF_Writeable);
}
void ProactorFile::uninit() {
    ASSERT(mConnectedCallback == nullptr);
    ASSERT(mAcceptCallback == nullptr);
    mReadBuf.uninit();
    mWriteBuf.uninit();
    mService->getPoller()->del(mFd);
    CLOSE(mFd);
}
void ProactorFile::onRead() {
    if (mAcceptCallback) {
        HostAddress addr;
        TCPSocket socket = TCPSocket::fromFd(mFd);
        socket = socket.accept(&addr);

        ProactorFile *file = mService->attachFd(socket.getFd());

        auto f = mAcceptCallback;
        mAcceptCallback = nullptr;
        f(file, addr);
    } else {
        if (mService->isBlocking()) mReadBuf.onReadBlocking();
        else mReadBuf.onReadNonblocking();
    }
}
void ProactorFile::onWrite() {
    if (mConnectedCallback) {
        auto f = mConnectedCallback;
        mConnectedCallback = nullptr;

        TCPSocket socket = TCPSocket::fromFd(mFd);
        P_ENSURE(socket.getOption<int>(SO_ERROR) == 0);
        socket.setNonBlocking(!mService->isBlocking());

        mService->getPoller()->update(mFd, this, IPoller::EF_Readable);

        f(this);
    } else {
        if (mService->isBlocking()) mWriteBuf.onWriteBlocking();
        else mWriteBuf.onWriteNonblocking();

        if (!mWriteBuf.hasPendingData()) {
            mService->getPoller()->update(mFd, this, IPoller::EF_Readable);
        }
    }
}
