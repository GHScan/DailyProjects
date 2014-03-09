#include "pch.h"

#include <vector>

#include <unistd.h>

#include "Proactor.h"

static short g_port = 7788;
static bool g_blocking = true;
static const char *g_pollerType = "select"; 

class Connection {
public:
    Connection(ProactorFile *socket, const HostAddress &addr): 
        mSrcSocket(socket), mDestSocket(nullptr), mRespForwardedPos(0), mCountOfEOF(0) {

        mID = format("Connection(%d,%s)", socket->getFd(), addr.toString().c_str());

        LOG("Connection(%s) established ...", getID());

        mSrcSocket->readLine('\n', [this](bool eof, const char *buf, int n){ onReadRequestline(eof, buf, n); });
    }
    ~Connection() {
        LOG("Connection(%s) finished. response length=%d", getID(), mResponse.size());

        if (mDestSocket != nullptr)  mDestSocket->destroy();
    }
private:
    void onReadRequestline(bool eof, const char *buf, int n) {
        if (eof) {
            LOG_ERR("Connection(%s) found eof too early !", getID());
            mSrcSocket->destroy();
            return;
        }

        char method[32], version[32], url[1024];
        if (sscanf(string(buf, buf + n).c_str(), "%s %s %s", method, url, version) != 3) {
            LOG_ERR("Connection(%s) parse request line failed !", getID());
            mSrcSocket->destroy();
            return;
        }

        mUrl = url;
        if ((int)mUrl.find("//") == -1) mUrl = "http://" + mUrl;
        if (mUrl.find("http://") != 0) {
            LOG_ERR("Connection(%s) only support http forwarding !", getID());
            mSrcSocket->destroy();
            return;
        }
        mHost = mUrl.substr(7);
        if ((int)mHost.find('/') != -1) {
            mHost = mHost.substr(0, mHost.find('/'));
        }

        LOG("Connection(%s) request line: method=%s, version=%s, host=%s, url=%s", getID(), method, version, mHost.c_str(), url);

        mRequest.assign(buf, buf + n);
        mRequest.push_back('\n');

        mSrcSocket->getService()->createClientSocket(HostAddress::parse(mHost.c_str(), 80), [this](ProactorFile *socket){
            onDestSocketConnected(socket);
        });
    }
    void onDestSocketConnected(ProactorFile *socket) {
        mDestSocket = socket;
        mDestSocket->writeN(&mRequest[0], (int)mRequest.size(), [this](){ onForwardRequest(); });
        mDestSocket->readSome([this](bool eof, const char *buf, int n){ onReadResponse(eof, buf, n); });
    }
    void onReadRequest(bool eof, const char *buf, int n) {
        if (eof) {
            if (++mCountOfEOF == 2) {
                mSrcSocket->destroy();
            }
            return;
        }

        mRequest.assign(buf, buf + n);
        mRequest.push_back('\n');

        mDestSocket->writeN(&mRequest[0], (int)mRequest.size(), [this](){ onForwardRequest(); });
    }
    void onForwardRequest() {
        mSrcSocket->readSome([this](bool eof, const char *buf, int n){ onReadRequest(eof, buf, n); });
    }
    void onReadResponse(bool eof, const char *buf, int n) {
        if (eof) {
            if (++mCountOfEOF == 2) {
                mSrcSocket->destroy();
            }
            return;
        }

        mResponse.insert(mResponse.end(), buf, buf + n);

        int newForwardPos = (int)mResponse.size();
        mSrcSocket->writeN(&mResponse[mRespForwardedPos], newForwardPos - mRespForwardedPos, [this, newForwardPos](){
            mRespForwardedPos = newForwardPos;
            onForwardResponse();
        });
    }
    void onForwardResponse() {
        mDestSocket->readSome([this](bool eof, const char *buf, int n){ onReadResponse(eof, buf, n); });
    }
private:
    const char *getID() { return mID.c_str(); }
private:
    string mID;
    ProactorFile *mSrcSocket;
    ProactorFile *mDestSocket;
    string mUrl;
    string mHost;
    vector<char> mRequest;
    vector<char> mResponse; // TO optimize
    int mRespForwardedPos;
    int mCountOfEOF;
};

static bool parseAppArgs(int argc, char *argv[]) {
    ILogger::instance()->suppressLog(true);

    int opt;
    while ((opt = getopt(argc, argv, "vP:p:b")) != -1) {
        switch (opt) {
            case 'P':
                g_port = atoi(optarg);
                break;
            case 'v':
                ILogger::instance()->suppressLog(false);
                break;
            case 'p':
                g_pollerType = optarg;
                break;
            case 'b':
                g_blocking = false;
                break;
            default:
                LOG_ERR("Usage : %s [-p poller_type] [-P port] [-v] [-b]", argv[0]);
                return false;
        }
    }

    return true;
}

int main(int argc, char *argv[]) {
    if (!parseAppArgs(argc, argv)) return 1;

    ProactorService service(g_pollerType, g_blocking);

    ProactorFile *input = service.attachFd(::dup(STDIN_FILENO));
    ON_EXIT_SCOPE([input](){ input->destroy(); });
    bool shouldExit = false;
    input->readLine('\n', [&shouldExit](bool eof, const char *buf, int n){ shouldExit = true; });

    ProactorFile *listenSocket = service.createListenSocket(HostAddress::fromLocal(g_port), 32);
    ON_EXIT_SCOPE([listenSocket](){ listenSocket->destroy(); });
    function<void(ProactorFile*,const HostAddress&)> onAccept =
    [listenSocket, &onAccept](ProactorFile *socket, const HostAddress &addr) {
        Connection *c = new Connection(socket, addr);
        socket->setDestroyCallback([c](){ delete c; });

        listenSocket->accept(onAccept);
    };
    listenSocket->accept(onAccept);

    LOG_ERR("Listen on port %d", g_port);
    LOG_ERR_MSG("Press any key to exit...");

    while (!shouldExit) {
        try {
            service.wait(300);
        } catch(const RuntimeException &e) {
            LOG_ERR("Found runtime exception %s", e.what());
        }
    }
}
