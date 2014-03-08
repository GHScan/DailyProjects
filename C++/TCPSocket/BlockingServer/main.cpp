#include "pch.h"

#include <map>

#include <unistd.h>
#include <signal.h>

#include "Utils.h"
#include "Threading.h"
#include "IO.h"
#include "Socket.h"
#include "Poller.h"

bool g_shouldExit;

class FileServer {
public:
    const vector<char>* getFileContent(const char *path) {
        {
            RLockGuard guard(mLock);
            auto iter = mFiles.find(path);
            if (iter != mFiles.end()) return iter->second;
        }
        {
            WLockGuard guard(mLock);
            auto iter = mFiles.find(path);
            if (iter != mFiles.end()) return iter->second;
            auto p = new vector<char>();
            readFile(path, *p);
            mFiles[path] = p;
            return p;
        }
    }
    ~FileServer() {
        WLockGuard guard(mLock);
        for (auto &p : mFiles) delete p.second;
    }
private:
    map<string, vector<char>*> mFiles;
    RWLock mLock;
} g_fileServer;

static void handleHTTPRequest(const TCPSocket &socket) {
    BlockingReadBuffer readBuf(socket.getFd(), 512);
    string line;

    if (!readBuf.readLine(line)) {
        LOG_ERR_MSG("Read line failed!");
        return;
    }

    char method[32], url[128], version[32];
    if (sscanf(line.c_str(), "%s %s %s", method, url, version) != 3) {
        LOG_ERR_MSG("Parse http request line failed!");
        return;
    }

    while (!trimString(line.c_str()).empty()) {
        if (!readBuf.readLine(line)) {
            LOG_ERR_MSG("Skip http header failed!");
            return;
        }
    }

    const vector<char> *data = g_fileServer.getFileContent(url);
    vector<char> empty;
    data = data != nullptr ? data : &empty;

    string response = format("HTTP/1.0 200 OK\nContent-Type: text/plain\nContent-Length: %d\n\n", data->size());

    BlockingIO::writeN(socket.getFd(), response.c_str(), response.size());
    BlockingIO::writeN(socket.getFd(), &(*data)[0], data->size());

    LOG("Recevie request : method=%s, version=%s, url=%s, body=%d", method, version, url, data->size());
}

static void serverThread(TCPSocket listenSocket, const char *pollerType) {
    Thread thread = Thread::current();

    IPoller *poller = IPoller::create(pollerType);
    ON_EXIT_SCOPE([&poller](){ DELETE(poller); });

    poller->add(listenSocket.getFd(), (void*)listenSocket.getFd(), IPoller::EF_Readable);
    auto _cleanup = [poller, listenSocket](){ poller->del(listenSocket.getFd()); };
    ON_EXIT_SCOPE(_cleanup);

    vector<IPoller::Event> events;
    while (!g_shouldExit) {
        if (!poller->wait(events, 300)) continue;

        for (auto &event : events) {
            TCPSocket socket(TCPSocket::fromFd((int)event.ud));

            if (event.flag & IPoller::EF_ErrFound) {
                LOG_ERR("Error found : fd=%d,%s", socket.getFd(), strerror(socket.getOption<int>(SO_ERROR)));
                poller->del(socket.getFd());
                socket.close();
            } else if (event.flag & IPoller::EF_Readable) {
                if (socket.getFd() == listenSocket.getFd()) {
                    HostAddress addr;
                    socket = socket.accept(&addr);
                    LOG("Accept client: tid=%d,fd=%d,%s", thread.getTid(), socket.getFd(), addr.toString().c_str());
                    poller->add(socket.getFd(), (void*)socket.getFd(), IPoller::EF_Readable);
                } else {

                    try {
                        handleHTTPRequest(socket);
                    } catch(const RuntimeException& e) {
                        LOG_ERR("Found runtime exception while handle http request: %s", e.what());
                    }

                    poller->del(socket.getFd());
                    socket.close();
                    LOG_MSG("Close client");
                }
            } 
        }
    }
}

int main(int argc, char *argv[]) {
    int threadCount = 1;
    short port = 7788;
    const char *pollerType = "select";

    setSignalHandler(SIGPIPE, SIG_IGN);

    ILogger::instance()->suppressLog(true);

    int opt;
    while ((opt = getopt(argc, argv, "P:n:p:vb")) != -1) {
        switch (opt) {
            case 'n':
                threadCount = atoi(optarg);
                break;
            case 'p':
                pollerType = optarg;
                break;
            case 'P':
                port = atoi(optarg);
                break;
            case 'v':
                ILogger::instance()->suppressLog(false);
                break;
            case 'b':
                break;
            default:
                LOG_ERR("%s [-n thread_count] [-P port] [-p poller] [-v]", argv[0]);
                return 1;
        }
    }

    TCPSocket listenSocket = TCPSocket::create();
    ON_EXIT_SCOPE([&listenSocket](){ listenSocket.close(); });

    listenSocket.setReuseAddress();
    listenSocket.bind(HostAddress::fromLocal(port));
    listenSocket.listen(32);

    Thread::create([]() {

            LOG_ERR("%s", "Press any key to exit server...");
            getchar();
            g_shouldExit = true;

            }, 16 * 1024).detach();

    if (threadCount == 1) {
        serverThread(listenSocket, pollerType);
    } else {
        vector<Thread> threads;
        for (int i = 0; i < threadCount; ++i) {
            Thread td = Thread::create([listenSocket, pollerType](){

                    serverThread(listenSocket, pollerType);

                    }, 16 * 1024);
            threads.push_back(td);
        }
        for (auto &td : threads) td.join();
    }
}
