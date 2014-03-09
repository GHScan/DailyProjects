#include "pch.h"

#include <map>

#include <unistd.h>

#include "Poller.h"
#include "Socket.h"
#include "IO.h"
#include "Threading.h"

struct Task {
    string host;
    string outputFile;
    string request;
};
istream& operator >> (istream &si, Task &t) {
    if (si) si >> t.host; 
    if (si) si >> t.outputFile;
    if (si) {
        int reqLineCount;
        si >> reqLineCount;
        string line;
        getline(si, line);
        t.request.clear();
        for (string line; reqLineCount-- > 0 && getline(si, line);) {
            t.request += line + '\n';
        }
    }
    return si;
}

class Client;
class ClientManager {
    DISABLE_COPY(ClientManager);
public:
    ClientManager(IPoller *poller);
    ~ClientManager();
    Client* createClient(const Task &task);
    void destroyClient(Client *c);
    void updateClientEvent(Client *c, int ef);
    void wait();
    int size() const { return mPoller->size(); }
private:
    IPoller *mPoller;
};

class Client {
    DISABLE_COPY(Client);
public:
    Client(ClientManager *mgr, const Task &task);
    ~Client();
    void onRead();
    void onWrite();
    void onError();
    int getFd() const  { return mSocket.getFd(); }
private:
    enum State {
        S_Connecting,
        S_WritingRequest,
        S_ReadingResponse,
    };
private:
    void onConnectComplete();
private:
    ClientManager *mMgr;
    TCPSocket mSocket;
    State mState;
    Task mTask;
    int mReqWritenSize;
    char mReadBuf[1 << 14];
    int mReadedSize;
    FILE *mOutFile;
};

ClientManager::ClientManager(IPoller *poller): mPoller(poller) {
}
ClientManager::~ClientManager() {
    DELETE(mPoller);
}
Client* ClientManager::createClient(const Task &task) {
    Client *c = new Client(this, task);
    mPoller->add(c->getFd(), c, IPoller::EF_Writeable);
    return c;
}
void ClientManager::destroyClient(Client *c) {
    mPoller->del(c->getFd());
    DELETE(c);
}
void ClientManager::updateClientEvent(Client *c, int ef) {
    mPoller->update(c->getFd(), c, ef);
}
void ClientManager::wait() {
    vector<IPoller::Event> events;
    if (!mPoller->wait(events, 1000)) return;

    for (auto &event : events) {
        Client *c = (Client*)event.ud;
        if (event.flag & IPoller::EF_Readable) c->onRead();
        if (event.flag & IPoller::EF_Writeable) c->onWrite();
        if (event.flag & IPoller::EF_ErrFound) c->onError();
    }
}

Client::Client(ClientManager *mgr, const Task &task): 
    mMgr(mgr), mTask(task), mReqWritenSize(0), mReadedSize(0), mOutFile(nullptr) {

    mSocket = TCPSocket::create();
    mSocket.setNonBlocking(true);
    mState = S_Connecting;
    if (mSocket.connectAsync(HostAddress::parse(task.host.c_str(), 80))) {
        onConnectComplete();
    } 
}
Client::~Client() {
    if (mOutFile != nullptr) fclose(mOutFile);
    mSocket.close();
}
void Client::onConnectComplete() {
    ASSERT(mState == S_Connecting);

    P_ENSURE_R(mSocket.getOption<int>(SO_ERROR));
    mState = S_WritingRequest;
    mReqWritenSize = 0;
    mSocket.setNonBlocking(false);
}
void Client::onRead() {
    LOG("%s", "begin onRead...");

    ASSERT(mState == S_ReadingResponse);

    int n = BlockingIO::readSome(mSocket.getFd(), mReadBuf + mReadedSize, sizeof(mReadBuf) - mReadedSize);
    mReadedSize += n;

    LOG("%p, Readed %d bytes", this, n);

    if (n == 0 || mReadedSize == sizeof(mReadBuf)) {
        if (mTask.outputFile == "stdout") {
            BlockingIO::writeN(STDOUT_FILENO, mReadBuf, mReadedSize);
        } else {
            if (mOutFile == nullptr) {
                mOutFile = fopen(mTask.outputFile.c_str(), "wb");
            }
            P_ENSURE(mOutFile != nullptr);
            BlockingIO::writeN(fileno(mOutFile), mReadBuf, mReadedSize);
        } 
        mReadedSize = 0;
    }

    if (n == 0) {
        mMgr->destroyClient(this);
    }
}
void Client::onWrite() {
    LOG("%s", "begin onWrite...");

    if (mState == S_Connecting) {
        onConnectComplete();
    }

    ASSERT(mState == S_WritingRequest);

    const string &request = mTask.request;
    int n = BlockingIO::writeSome(mSocket.getFd(), request.c_str() + mReqWritenSize, (int)request.size() - mReqWritenSize);

    LOG("%p Write %d bytes", this, n);

    mReqWritenSize += n;
    if (mReqWritenSize == (int)request.size()) {
        mState = S_ReadingResponse;
        mReadedSize = 0;
        mMgr->updateClientEvent(this, IPoller::EF_Readable);
    }
}
void Client::onError() {
    LOG_ERR("onError : %s\n", strerror(mSocket.getOption<int>(SO_ERROR)));
    mMgr->destroyClient(this);
}

static void handleTaskList(const Task *begin, const Task *end, int maxActiveTask, const char *pollerType) {
    ClientManager mgr(IPoller::create(pollerType));
    while (begin != end || mgr.size() > 0) {
        try {
            while (begin != end && mgr.size() < maxActiveTask) {
                mgr.createClient(*begin++);
            }

            mgr.wait();
        } catch(const RuntimeException& e) {
            LOG_ERR("Found runtime exception while handling task list: %s\n", e.what());
        }
    }
}

int main(int argc, char *argv[]) {
    int threadCount = 1;
    int maxActiveTask = 1;
    const char *pollerType = "select";

    ILogger::instance()->suppressLog(true);

    int opt;
    while ((opt = getopt(argc, argv, "n:m:p:v")) != -1) {
        switch (opt) {
            case 'n':
                threadCount = atoi(optarg);
                break;
            case 'm':
                maxActiveTask = atoi(optarg);
                break;
            case 'p':
                pollerType = optarg;
                break;
            case 'v':
                ILogger::instance()->suppressLog(false);
                break;
            default:
                LOG_ERR("%s [-n thread_count] [-m max_active_task] [-p poller_type] [-v]", argv[0]);
                return 1;
        }
    }

    vector<Task> taskList;
    {
        Task task;
        while (cin >> task) taskList.push_back(task);
    }
    int taskCount = (int)taskList.size();
    int taskCountPerThread = (taskCount + threadCount - 1) / threadCount;

    LOG("task count=%d, task per thread=%d", taskCount, taskCountPerThread);

    vector<Thread> threads;
    for (int i = 0; i < threadCount; ++i) {
        int begin = i * taskCountPerThread;
        if (begin >= taskCount) break;
        int end = min(taskCount, (i + 1) * taskCountPerThread);

        auto td = Thread::create([&taskList, begin, end, maxActiveTask, pollerType](){
                    handleTaskList(&taskList[0] + begin, &taskList[0] + end, maxActiveTask, pollerType);
                }, 64 * 1024);
        threads.push_back(td);
    }
    for (auto &thread : threads) thread.join();
}
