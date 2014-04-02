#include "pch.h"

#include <stdarg.h>

#include <execinfo.h>
#include <unistd.h>
#include <signal.h>

#include "Utils.h"
#include "IO.h"
#include "Threading.h"

const char *_EXCEPTION_CTX_A = "";
const char *_EXCEPTION_CTX_B = "";

LogicError::LogicError(const char *describ, const char *file, int line)
    : Throwable(constructErrorMsg(describ, file, line).c_str()){
}
string LogicError::constructErrorMsg(const char *describ, const char *file, int line) {
    int skipFrame = 3;
#ifdef NDEBUG
    skipFrame = 0;
#endif
    return format("%s(%d): %s\n", file, line, describ) + traceStack(skipFrame);
}

PosixException::PosixException(int err, const char *describ, const char *file, int line)
    : RuntimeException(constructErrorMsg(err, describ, file, line).c_str()) {
}
string PosixException::constructErrorMsg(int err, const char *describ, const char *file, int line) {
    string s = format("%s(%d): %s\n\t%s\n", file, line, describ, strerror(err));
#ifndef NDEBUG
    s += traceStack(3);
#endif
    return s;
}

string format(const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    string v(256, 0);
    while (vsnprintf((char*)v.c_str(), v.size(), fmt, args) >= (int)v.size()) {
        v.resize(v.size() * 3 / 2);
    }
    v.resize(strlen(v.c_str()));
    va_end(args);
    return v;
}

string trimString(const char *str) {
    const char *begin = str;
    const char *end = begin + strlen(begin);
    while (begin < end && isspace(begin[0])) ++begin;
    while (begin < end && isspace(end[-1])) --end;
    return string(begin, end);
}

string cmdOpenAndRetrieve(const char **args, const char *input) {
    int writePipe[2], readPipe[2];
    P_ENSURE(::pipe(writePipe) == 0);
    P_ENSURE(::pipe(readPipe) == 0);
    auto _cleanup = [&readPipe, &writePipe](){
        for (int i = 0; i < 2; ++i) {
            CLOSE(writePipe[i]);
            CLOSE(readPipe[i]);
        }
    };
    ON_EXIT_SCOPE(_cleanup);

    int pid = ::fork();
    P_ENSURE(pid >= 0);
    if (pid == 0) {
        ::dup2(writePipe[0], STDIN_FILENO);
        ::dup2(readPipe[1], STDOUT_FILENO);
        CLOSE(writePipe[1]);
        CLOSE(readPipe[0]);
        P_ENSURE(::execvp(args[0], (char *const*)args) != -1);
    }

    CLOSE(writePipe[0]);
    CLOSE(readPipe[1]);

    BlockingIO::writeN(writePipe[1], input, strlen(input));
    CLOSE(writePipe[1]);

    string ret;
    char buf[512];
    int n;
    while ((n = BlockingIO::readSome(readPipe[0], buf, sizeof(buf))) > 0) {
        buf[n] = 0;
        ret += buf;
    }
    return ret;
}

int getCpuCount() {
    int count = ::sysconf(_SC_NPROCESSORS_ONLN);
    P_ENSURE(count != -1);
    return count;
}

bool readFile(const char *path, vector<char> &buf) {
    FILE *f = fopen(path, "rb");
    if (f == nullptr) return false;
    ON_EXIT_SCOPE([f](){ fclose(f); });

    fseek(f, 0, SEEK_END);
    int size = ftell(f);
    fseek(f, 0, SEEK_SET);

    buf.resize(size);
    if (size > 0) {
        P_ENSURE(fread(&buf[0], size, 1, f) > 0);
    }

    return true;
}
string traceStack(int skipFrame) { 
    string ret;

    void *frames[32];
    int n = ::backtrace(frames, ARRAY_SIZE(frames));
    char **strs = ::backtrace_symbols(frames, n);
    for (int i = skipFrame; i < n; ++i) { 
        ret += strs[i]; 
        ret += '\n';
    }
    free(strs);

#ifndef NDEBUG
    char curExecName[256] = "";
    if (::readlink("/proc/self/exe", curExecName, sizeof(curExecName) - 1) == -1) {
        strcpy(curExecName, "./main");
    }
    string bashArgs = 
        format(R"***(grep -oP '\[0x\w+\]' | grep -oP '0x\w+' | xargs addr2line -e '%s')***", curExecName);
    const char *args[] = {
        "bash",
        "-c",
        bashArgs.c_str(),
        nullptr,
    };
    ret = cmdOpenAndRetrieve(args, ret.c_str());
#endif

    return ret;
}

SigHandlerT setSignalHandler(int signum, SigHandlerT handler) {
    struct sigaction act = {0}, old = {0};
    P_ENSURE(::sigemptyset(&act.sa_mask) == 0);
    act.sa_flags = SA_RESTART;
    act.sa_handler = handler;
    P_ENSURE(::sigaction(signum, &act, &old) == 0);
    return old.sa_handler;
}

class Logger: public ILogger {
    DISABLE_COPY(Logger);
public:
    Logger(): mSuppressLog(false), mIdx(0) {}
    virtual void suppressLog(bool b) {
        mSuppressLog = b;
    }
    virtual void log(const char *msg) {
        if (!mSuppressLog) _stderrPrint(msg);
    }
    virtual void logErr(const char *msg) {
        if (isatty(STDOUT_FILENO)) {
            _stderrPrint(format("\x1b[01;40;31m%s\x1b[0m", msg).c_str());
        } else {
            _stderrPrint(msg);
        }
    }
private:
    void _stderrPrint(const char *msg) {
        LockGuard guard(mMutex);

        char buf[32] = "";
        time_t now; 
        time(&now);
        tm _tm;
        strftime(buf, sizeof(buf), "%F %T", localtime_r(&now, &_tm));

        fprintf(stdout, "[%d][%s] %s\n", mIdx++, buf, msg);
        fflush(stdout);
    }
private:
    Mutex mMutex;
    bool mSuppressLog;
    int mIdx;
};

ILogger* ILogger::instance() {
    static Logger s_logger;
    return &s_logger;
}
