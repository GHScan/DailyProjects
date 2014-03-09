#include "pch.h"

#include <unistd.h>
#include <sys/wait.h>

#include "Socket.h"
#include "IO.h"

static void tcpOpenAndRetrieve(const char *host, const char *request, int reqlen, vector<char> &response) {
    ASSERT(response.empty());
    LOG_MSG("Connect start...");

    TCPSocket sock = TCPSocket::create();
    ON_EXIT_SCOPE([&sock](){ sock.close(); });

    sock.connect(HostAddress::parse(host, 80));
    BlockingIO::writeN(sock.getFd(), request, reqlen);

    char buf[16 * 1024];
    for (int n = 0; (n = BlockingIO::readSome(sock.getFd(), buf, sizeof(buf))) > 0; ) {
        response.insert(response.end(), buf, buf + n);
    }

    LOG("Connect end (%d).", response.size());
} 

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

static void handleTaskList(const Task *begin, const Task *end) {
    vector<char> buf;
    buf.reserve(64 * 1024);

    for (; begin < end; ++begin) {
        const Task &task = *begin;
        try {
            buf.clear();
            tcpOpenAndRetrieve(task.host.c_str(), task.request.empty() ? "" : task.request.c_str(), task.request.size(), buf);
            if (buf.empty()) continue;

            if (task.outputFile == "stdout") {
                cout << &buf[0];
            } else {
                FILE *f = fopen(task.outputFile.c_str(), "wb");
                P_ENSURE(f != nullptr);
                fwrite(&buf[0], buf.size(), 1, f);
                fclose(f);
            }
        } catch(const RuntimeException& e) {
            LOG_ERR("Exception found while task running: %s", e.what());
        }
    }
}

int main(int argc, char *argv[]) {
    int processCount = 1;

    ILogger::instance()->suppressLog(true);

    int opt;
    while ((opt = getopt(argc, argv, "n:m:v")) != -1) {
        switch (opt) {
            case 'n':
                processCount = atoi(optarg);
                break;
            case 'm':
                break;
            case 'v':
                ILogger::instance()->suppressLog(false);
                break;
            default:
                LOG_ERR("%s [-n process_count] [-v]", argv[0]);
                return 1;
        }
    }

    vector<Task> taskList;
    for (Task task; cin >> task; ) taskList.push_back(task);
    if (taskList.empty()) return 0;

    int taskPerProcess = ((int)taskList.size() + processCount - 1) / processCount;

    const Task *begin = &taskList[0]; 
    const Task *end = begin + taskList.size();

    if (processCount == 1) {
        handleTaskList(begin, end);
    } else {
        for (; begin < end; begin += taskPerProcess) {
            if (fork() != 0) continue;
            const Task *curEnd = min(begin + taskPerProcess, end);
            handleTaskList(begin, curEnd);
            return 0;
        }

        while (wait(nullptr) > 0);
    }
}
