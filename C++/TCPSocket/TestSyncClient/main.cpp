#include "pch.h"

#include <assert.h>
#include <stdarg.h>
#include <string.h>
#include <errno.h>

#include <exception>

#include <unistd.h>
#include <netinet/in.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <sys/wait.h>
#include <netdb.h>

//////////////////////////////
#define _CONN(a, b) a##b
#define CONN(a, b) _CONN(a, b)

static char* format(const char *fmt, ...) {
    static char s_buf[256];
    va_list args;
    va_start(args, fmt);
    vsprintf(s_buf, fmt, args);
    va_end(args);
    return s_buf;
}

class PosixException: public exception {
public:
    PosixException(const char *exp, int err, const char *fname, int line) {
        mErrStr = format("%s\n\t%s(%d):%s\n", exp, fname, line, strerror(err));
    }
    const char* what() const throw() { return mErrStr.c_str(); }
private: 
    string mErrStr;
};

#define P_ASSERT(b, err) if (b); else throw PosixException(#b, err, __FILE__, __LINE__)

class ExitScope {
public:
    ExitScope(function<void()> f):mF(f) { }
    ~ExitScope() { if (mF) mF(); }
    void dismiss() { mF = nullptr; }
private:
    function<void()> mF;
};
#define EXIT_SCOPE(f) ExitScope CONN(__scope_, __LINE__)(f);

//////////////////////////////
static int writen(int fd, const char *buf, int len) {
    const char *cur = buf;
    while (len > 0) {
        int n = ::write(fd, cur, len);
        if (n <= 0)  {
            if (errno == EINTR) continue;
            P_ASSERT(0, errno);
        } else {
            cur += n;
            len -= n;
        }
    }
    return cur - buf;
}

static int readn(int fd, char *buf, int len) {
    char *cur = buf;
    while (len > 0) {
        int n = ::read(fd, cur, len);
        if (n == 0) {
            return cur - buf;
        } else if (n < 0) {
            if (errno == EINTR) continue;
            P_ASSERT(0, errno);
        } else {
            cur += n;
            len -= n;
        }
    }
    return cur - buf;
}
static void tcpOpenAndRetrieve(const char *ip, short port, const char *request, int reqlen, vector<char> &response) {
    char buf[1 << 14];

    int sockfd = ::socket(AF_INET, SOCK_STREAM, 0);
    P_ASSERT(sockfd != -1, errno);
    EXIT_SCOPE([&](){ ::close(sockfd); });

    in_addr ipaddr;
    if (::inet_aton(ip, &ipaddr) == 0) {
        hostent ret = {0}, *pret;
        int err = 0;
        if (::gethostbyname_r(ip, &ret, buf, sizeof(buf), &pret, &err) != 0) {
            P_ASSERT(0, err);
        }
        for (in_addr_t **addrs = (in_addr_t**)ret.h_addr_list; *addrs; ++addrs) {
            ipaddr.s_addr = **addrs;
            break;
        }
    }

    sockaddr_in addr;
    addr.sin_family = AF_INET;
    addr.sin_port = htons(port);
    addr.sin_addr = ipaddr;
    if (::connect(sockfd, (sockaddr*)&addr, sizeof(addr)) == -1) {
        P_ASSERT(0, errno);
    }

    writen(sockfd, request, reqlen);

    for (int n = 0; (n = readn(sockfd, buf, sizeof(buf))) > 0; ) {
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
        tcpOpenAndRetrieve(mIP.c_str(), mPort, mRequest.empty() ? "" : mRequest.c_str(), mRequest.size(), buf);
        if (buf.empty()) return;
        if (mOutFile == "stdout") {
            cout << &buf[0];
        } else {
            FILE *f = fopen(mOutFile.c_str(), "wb");
            P_ASSERT(f != NULL, errno);
            fwrite(&buf[0], buf.size(), 1, f);
            fclose(f);
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
        fprintf(stderr, "Usage : %s process_count", argv[0]);
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
