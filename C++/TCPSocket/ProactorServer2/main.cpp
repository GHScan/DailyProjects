#include "pch.h"

#include <map>
#include <memory>

#include <unistd.h>
#include <signal.h>

#include "Proactor.h"

class FileServer {
public:
    const vector<char>* getFileContent(const char *path) {
        auto iter = mFiles.find(path);
        if (iter != mFiles.end()) return iter->second;
        auto p = new vector<char>();
        if (!readFile(path, *p)) {
            DELETE(p);
        }
        mFiles[path] = p;
        return p;
    }
    ~FileServer() {
        for (auto &p : mFiles) delete p.second;
    }
private:
    map<string, vector<char>*> mFiles;
} g_fileServer;

class Client {
public:
    Client(ProactorFile *file, const HostAddress& addr):
        mFile(file), mAddr(addr) {

        LOG("%s start...", getID().c_str());

        mFile->readLine('\n', [this](bool eof, const char *buf, int len){
            onRequestLineRead(eof, buf, len);
        });
    }
    ~Client() {
        LOG("%s exit...", getID().c_str());
    }
private:
    void onRequestLineRead(bool eof, const char *buf, int len) {
        if (eof) {
            LOG_ERR("%s found EOF while parsing request line...", getID().c_str());
            mFile->destroy();
            return;
        }

        char method[32], version[32], url[128];
        if (sscanf(string(buf, buf + len).c_str(), "%s %s %s", method, url, version) != 3) {
            LOG_ERR("%s parse request line failed!", getID().c_str());
            mFile->destroy();
            return;
        }

        mUrl = url;

        mFile->readLine('\n', [this](bool eof, const char *buf, int len){
            onRequestHeaderRead(eof, buf, len);
        });
    }
    void onRequestHeaderRead(bool eof, const char *buf, int len) {
        if (eof) {
            LOG_ERR("%s found EOF while skip request header...", getID().c_str());
            mFile->destroy();
            return;
        }

        if (!trimString(string(buf, buf + len).c_str()).empty()) {
            mFile->readLine('\n', [this](bool eof, const char *buf, int len){
                onRequestHeaderRead(eof, buf, len);
            });
        } else {
            mResponseBody = g_fileServer.getFileContent(mUrl.c_str());

            if (mResponseBody == nullptr) {
                LOG_ERR("%s file or directory not found : %s", getID().c_str(), mUrl.c_str());
                mFile->destroy();
                return;
            } else {
                mResponseHeader = format("HTTP/1.0 200 OK\nContent-Type: text/plain\nContent-Length: %d\n\n", mResponseBody->size());
                mFile->writeN(mResponseHeader.c_str(), mResponseHeader.size(), [this](){
                    onResponseHeadersWriten();
                });
            }
        }
    }
    void onResponseHeadersWriten() {
        if (mResponseBody->empty()) onResponseBodyWriten();
        else {
            mFile->writeN(&(*mResponseBody)[0], mResponseBody->size(), [this](){
                onResponseBodyWriten();
            });
        }
    }
    void onResponseBodyWriten() {
        LOG("%s send response successed ! %d,%s", getID().c_str(), mResponseBody->size(), mUrl.c_str());
        mFile->destroy();
    }
private:
    string getID() const { return format("Client(%d,%s)", mFile->getFd(), mAddr.toString().c_str()); }
private:
    ProactorFile *mFile;
    HostAddress mAddr;
    string mUrl;
    string mResponseHeader;
    const vector<char> *mResponseBody;
};

class Server {
public:
    Server(int argc, char *argv[]): mShouldExit(false) {
        setSignalHandler(SIGPIPE, SIG_IGN);

        ILogger::instance()->suppressLog(true);

        const char *pollerType = "select";
        int port = 7788;
        bool blocking = true;

        int opt;
        while ((opt = getopt(argc, argv, "n:P:p:vb")) != -1) {
            switch (opt) {
                case 'n':
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
                    blocking = false;
                    break;
                default:
                    LOG_ERR("%s [-P port] [-p poller] [-v] [-b]", argv[0]);
                    exit(1);
            }
        }

        mService = new ProactorService(pollerType, blocking);

        LOG_ERR_MSG("Press any key to exit server ...");
        mInputFile = mService->attachFd(::dup(STDIN_FILENO));
        mInputFile->readLine('\n', [this](bool eof, const char *buf, int n){
            mShouldExit = true;
        });

        mListenSocket = mService->createListenSocket(HostAddress::fromLocal(port), 32);
        mListenSocket->accept([this](ProactorFile *file, const HostAddress& addr){
            onAcceptClient(file, addr);
        });
    }
    ~Server() {
        mInputFile->destroy();
        mListenSocket->destroy();

        DELETE(mService);
    }
    void run() {
        while (!mShouldExit) mService->wait(300);
    }
private:
    void onAcceptClient(ProactorFile *file, const HostAddress& addr) {
        Client *c = new Client(file, addr);
        file->setDestroyCallback([c](){ delete c; });

        mListenSocket->accept([this](ProactorFile *file, const HostAddress& addr){
            onAcceptClient(file, addr);
        });
    }
private:
    ProactorService *mService;
    ProactorFile *mListenSocket;
    ProactorFile *mInputFile;
    bool mShouldExit;
};

int main(int argc, char *argv[]) {
    Server server(argc, argv);
    server.run();
}
