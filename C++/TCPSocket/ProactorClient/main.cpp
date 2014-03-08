#include "pch.h"

#include <map>

#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "Threading.h"
#include "Proactor.h"

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

static bool g_blocking = true;
static const char *g_pollerType = "select";
static int g_maxActiveTask = 1;
static int g_threadCount = 1;

static void handleTaskList(const Task *begin, const Task *end) {
    ProactorService service(g_pollerType, g_blocking);

    while (begin != end || service.size() > 0) {
        while (begin != end && service.size() < g_maxActiveTask) {
            const Task &task = *begin++;
            service.createClientSocket(HostAddress::parse(task.host.c_str(), 80), [&task](ProactorFile *client){
                client->writeN(task.request.c_str(), task.request.size(), [&task, client](){
                    TCPSocket::fromFd(client->getFd()).shutdownWr();

                    int outFd = task.outputFile == "stdout" ? STDOUT_FILENO : ::open(task.outputFile.c_str(), O_WRONLY | O_TRUNC | O_CREAT, S_IRUSR | S_IWUSR);
                    P_ENSURE(outFd != -1);

                    auto onBlockRead = new function<void(bool, const char*, int)>();

                    client->setDestroyCallback([outFd, onBlockRead](){ 
                        if (outFd != STDOUT_FILENO) ::close(outFd); 
                        delete onBlockRead;
                    });

                    const int BLOCK_SIZE = 32 * 1024;
                    *onBlockRead = [client, &task, onBlockRead, outFd](bool eof, const char *buf, int n) {
                        if (eof) {
                            LOG("Complete download : ->%s(%d)", task.outputFile.c_str(), outFd);
                            client->destroy();
                        } else {
                            LOG("read %d bytes", n);
                            BlockingIO::writeN(outFd, buf, n);
                            client->readN(BLOCK_SIZE, *onBlockRead);
                        }
                    };
                    client->readN(BLOCK_SIZE, *onBlockRead);

                });
            });
        }

        try {
            service.wait(1000);
        } catch (const RuntimeException& e) {
            LOG_ERR("Found runtime exception: %s", e.what());
        }
    }
}

int main(int argc, char *argv[]) {
    ILogger::instance()->suppressLog(true);

    int opt;
    while ((opt = getopt(argc, argv, "n:m:p:vb")) != -1) {
        switch (opt) {
            case 'n':
                g_threadCount = atoi(optarg);
                break;
            case 'm':
                g_maxActiveTask = atoi(optarg);
                break;
            case 'p':
                g_pollerType = optarg;
                break;
            case 'v':
                ILogger::instance()->suppressLog(false);
                break;
            case 'b':
                g_blocking = false;
                break;
            default:
                LOG_ERR("%s [-n thread_count] [-m max_active_task] [-p poller_type] [-v] [-b]", argv[0]);
                return 1;
        }
    }

    vector<Task> taskList;
    for (Task task; cin >> task; ) taskList.push_back(task);
    if (taskList.empty()) return 0;

    int taskPerThread = ((int)taskList.size() + g_threadCount - 1) / g_threadCount;

    const Task *begin = &taskList[0]; 
    const Task *end = begin + taskList.size();

    if (g_threadCount == 1) {
        handleTaskList(begin, end);
    } else {
        vector<Thread> threads;
        for (; begin < end; begin += taskPerThread) {
            const Task *curEnd = min(begin + taskPerThread, end);
            threads.push_back(Thread::create([begin, curEnd](){
                handleTaskList(begin, curEnd);
            }, 64 * 1024));
        }
        for (auto td : threads) td.join();
    }
}
