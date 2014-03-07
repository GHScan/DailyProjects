#include "pch.h"

#include <unistd.h>
#include <sys/wait.h>

#include "Socket.h"
#include "IO.h"

static void tcpOpenAndRetrieve(const char *ip, short port, const char *request, int reqlen, vector<char> &response) {
    TCPSocket sock = TCPSocket::create();
    ON_EXIT_SCOPE([&sock](){ sock.close(); });

    sock.connect(HostAddress::parse(ip, port));
    BlockingIO::writeN(sock.getFd(), request, reqlen);

    char buf[1 << 14];
    for (int n = 0; (n = BlockingIO::readSome(sock.getFd(), buf, sizeof(buf))) > 0; ) {
        response.insert(response.end(), buf, buf + n);
    }
} 

class Task {
public:
    istream& parse(istream &si) {
        if (!parseHost(si)) return si;
        if (!parseOutFile(si)) return si;
        if (!parseRequest(si)) return si;
        return si;
    }
    void run(vector<char>& buf) {
        try {

            tcpOpenAndRetrieve(mIP.c_str(), mPort, mRequest.empty() ? "" : mRequest.c_str(), mRequest.size(), buf);
            if (buf.empty()) return;
            if (mOutFile == "stdout") {
                cout << &buf[0];
            } else {
                FILE *f = fopen(mOutFile.c_str(), "wb");
                P_ENSURE(f != NULL);
                fwrite(&buf[0], buf.size(), 1, f);
                fclose(f);
            }

        } catch(const RuntimeException& e) {
            LOG_ERR("Exception found while task running: %s", e.what());
        }
    } 
private:
    bool parseHost(istream &si) {
        string header;
        si >> header >> mIP;
        if (!si) return false;
        assert(header == "host:");
        const char *p = strchr(mIP.c_str(), ':');
        mPort = atoi(p + 1);
        mIP.resize(p - mIP.c_str());
        return true;
    }
    bool parseOutFile(istream &si) {
        string header;
        si >> header >> mOutFile;
        if (!si) return false;
        assert(header == "output:");
        if (mOutFile == "null") mOutFile.clear();
        return true;
    }
    bool parseRequest(istream &si) {
        string header;
        int lineCount;
        si >> header >> lineCount;
        if (!si) return false;
        assert(header == "request:");
        getline(si, header);
        mRequest.clear();
        for (string line; lineCount > 0 && getline(si, line); --lineCount) {
            mRequest += line + '\n';
        }
        return true;
    }

private:
    string mIP;
    int mPort;
    string mOutFile;
    string mRequest;
};
istream& operator >> (istream& si, Task& t) {
    return t.parse(si);
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        LOG_ERR("Usage : %s process_count", argv[0]);
        return 1;
    }

    int processCount = atoi(argv[1]);

    vector<Task> taskList;
    {
        Task t;
        while (cin >> t) taskList.push_back(t);
    }

    vector<char> buf;
    buf.reserve(1 << 16);

    int step = ((int)taskList.size() + processCount - 1) / processCount;
    for (int i = 0; i < processCount - 1; ++i) {
        if (fork() != 0) continue;
        for (int j = 0; j < step; ++j) {
            int idx = i * step + j;
            if (idx >= (int)taskList.size()) break;
            buf.clear();
            taskList[i * step + j].run(buf);
        }
        return 0;
    }

    for (int i = (processCount - 1) * step; i < (int)taskList.size(); ++i) {
        buf.clear();
        taskList[i].run(buf);
    }

    while (wait(NULL) > 0);
}
