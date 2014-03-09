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

static short g_port = 7788;
static const char *g_pollerType = "select";
static bool g_blocking = true;
static bool g_shouldExit;

int main(int argc, char *argv[]) {

    setSignalHandler(SIGPIPE, SIG_IGN);

    ILogger::instance()->suppressLog(true);

    int opt;
    while ((opt = getopt(argc, argv, "n:P:p:vb")) != -1) {
        switch (opt) {
            case 'n':
                break;
            case 'p':
                g_pollerType = optarg;
                break;
            case 'P':
                g_port = atoi(optarg);
                break;
            case 'v':
                ILogger::instance()->suppressLog(false);
                break;
            case 'b':
                g_blocking = false;
                break;
            default:
                LOG_ERR("%s [-n thread_count] [-P port] [-p poller] [-v] [-b]", argv[0]);
                return 1;
        }
    }

    ProactorService service(g_pollerType, g_blocking);

    ProactorFile *listenSocket = service.createListenSocket(HostAddress::fromLocal(g_port), 32);
    ON_EXIT_SCOPE([listenSocket](){ listenSocket->destroy(); });
    function<void(ProactorFile*, const HostAddress&)> onAccept = [listenSocket, &onAccept](ProactorFile *client, const HostAddress& addr){
        LOG("Accept client : %d,%s", client->getFd(), addr.toString().c_str());

         auto onReadReqLine = [client](bool eof, const char *buf, int n) {
            if (eof) {
                LOG_ERR_MSG("EOF too early!");
                client->destroy();
                return;
            }

            string reqline(buf, buf + n);
            char method[32], version[32], url[128];
            if (sscanf(reqline.c_str(), "%s %s %s", method, url, version) != 3) {
                LOG_ERR_MSG("Parse request line failed!");
                client->destroy();
                return;
            }

            string urlStr(url);
            if (url[0] != '.') urlStr = '.' + urlStr;

            // TODO: onSkipHeader's lifetime is out of control!!!!
            function<void(bool,const char*, int)> onSkipHeader = [client, urlStr, &onSkipHeader](bool eof, const char *buf, int n) {
                if (eof) {
                    LOG_ERR_MSG("EOF to early!");
                    client->destroy();
                    return;
                }

                string line(buf, buf + n);
                if (!trimString(line.c_str()).empty()) {
                    client->readLine('\n', onSkipHeader);
                    return;
                }

                const vector<char> *data = g_fileServer.getFileContent(urlStr.c_str());
                if (data == nullptr) {
                    LOG_ERR("Can't find url : %s", urlStr.c_str());
                    client->destroy();
                    return;
                }

                string *response = new string(format("HTTP/1.0 200 OK\nContent-Type: text/plain\nContent-Length: %d\n\n", data->size()));
                client->writeN(response->c_str(), response->size(), [client, data, response, urlStr](){
                        delete response;

                        client->writeN(&(*data)[0], data->size(), [client, data, urlStr](){
                            LOG("Success full send response : %d,%s", data->size(), urlStr.c_str());
                            LOG_MSG("Close client");
                            client->destroy();
                        });
                });
            };
            client->readLine('\n', onSkipHeader);
        };
        client->readLine('\n', onReadReqLine);

        listenSocket->accept(onAccept);
    };
    listenSocket->accept(onAccept);

    ProactorFile *inputFile = service.attachFd(::dup(STDIN_FILENO));
    ON_EXIT_SCOPE([inputFile](){ inputFile->destroy(); });
    inputFile->readLine('\n', [](bool eof, const char *buf, int n){
        g_shouldExit = true;
    });
    LOG_ERR_MSG("Press any key to exit server ...");

    while (!g_shouldExit) service.wait(300);
}
