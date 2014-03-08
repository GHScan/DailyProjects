#ifndef PROACTOR_H
#define PROACTOR_H

#include <unordered_set>

#include "Poller.h"
#include "Socket.h"
#include "IO.h"
#include "Utils.h"

class ProactorFile;

class ProactorService {
    DISABLE_COPY(ProactorService);
public:
    ProactorService(const char *pollerType, bool blocking);
    ~ProactorService();

    void createClientSocket(const HostAddress &serverAddr, function<void(ProactorFile* file)> connectedCallback);
    ProactorFile* createListenSocket(const HostAddress &bindAddr, int backLog);
    ProactorFile* attachFd(int fd);
    void destroyFile(ProactorFile *file);

    IPoller* getPoller() { return mPoller; }
    bool isBlocking() const { return mBlocking; }
    int size() const { return (int)mActiveFiles.size(); }

    void wait(int timeout);
private:
    ProactorFile* allocFile();
    void freeFile(ProactorFile *file);
    void clearDeferDestroyFiles();
private:
    IPoller *mPoller;
    bool mBlocking;
    vector<ProactorFile*> mFreeFiles;
    vector<ProactorFile*> mDeferDestoryFiles;
    unordered_set<ProactorFile*> mActiveFiles;
};

class ProactorFile {
    DISABLE_COPY(ProactorFile);
public:
    void accept(function<void(ProactorFile* file, const HostAddress& addr)> callback);
    void readLine(char delmit, function<void(bool eof, const char *buf, int n)> callback);
    void readN(int n, function<void(bool eof, const char *buf, int n)> callback);
    void writeN(const char *buf, int n, function<void()> callback);
    int getFd() const { return mFd; }
    ProactorService* getService() { return mService; }
    void destroy();
    void setDestroyCallback(function<void()> callback);
private:
    ProactorFile(){}
    void init(ProactorService *service, int fd);
    void initWithConnectedCallbak(ProactorService *service, int fd, function<void(ProactorFile*)> callback);
    void uninit();
    void onRead();
    void onWrite();
    friend class ProactorService;
private:
    function<void()> mDestroyCallback;
    function<void(ProactorFile*)> mConnectedCallback;
    function<void(ProactorFile*, const HostAddress&)> mAcceptCallback;
    EventDrivenReadBuffer2 mReadBuf;
    EventDrivenWriteBuffer2 mWriteBuf;
    ProactorService *mService;
    int mFd;
};

#endif
