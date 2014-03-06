#include "pch.h"

#include <stdarg.h>

#include <execinfo.h>
#include <unistd.h>

#include "Utils.h"
#include "IO.h"
#include "Threading.h"

const char *_EXCEPTION_CTX_A = "";
const char *_EXCEPTION_CTX_B = "";

LogicError::LogicError(const char *describ, const char *file, int line)
    : Throwable(constructErrorMsg(describ, file, line).c_str()){
}
string LogicError::constructErrorMsg(const char *describ, const char *file, int line) {
    return format("%s(%d): %s\n", file, line, describ) + stackTrace();
}
string LogicError::stackTrace() {
    string ret;

    void *frames[32];
    int n = ::backtrace(frames, ARRAY_SIZE(frames));
    char **strs = ::backtrace_symbols(frames, n);
#ifndef NDEBUG
    // skip the frames in exception class
    for (int i = 3; i < n; ++i) { 
#else
    for (int i = 0; i < n; ++i) {
#endif
        ret += strs[i]; 
        ret += '\n';
    }
    free(strs);

#ifndef NDEBUG
    char curExecName[256];
    if (::readlink("/proc/self/exe", curExecName, sizeof(curExecName)) == -1) {
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

PosixException::PosixException(int err, const char *describ, const char *file, int line)
    : RuntimeException(format("%s(%d): %s\n\t%s\n", file, line, describ, strerror(err)).c_str()) {
}

string format(const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    string v(256, 0);
    while (vsnprintf((char*)v.c_str(), v.size(), fmt, args) == (int)v.size()) {
        v.resize(v.size() * 3 / 2);
    }
    v.resize(strlen(v.c_str()));
    va_end(args);
    return v;
}

string cmdOpenAndRetrieve(const char **args, const char *input) {
    int writePipe[2], readPipe[2];
    P_ENSURE(::pipe(writePipe) == 0);
    P_ENSURE(::pipe(readPipe) == 0);
    auto cleanup = [readPipe, writePipe](){
        for (int i = 0; i < 2; ++i) {
            if (writePipe[i] != -1) ::close(writePipe[i]);
            if (readPipe[i] != -1) ::close(readPipe[i]);
        }
    };
    ON_EXIT_SCOPE(cleanup);

    int pid = ::fork();
    P_ENSURE(pid >= 0);
    if (pid == 0) {
        ::dup2(writePipe[0], STDIN_FILENO);
        ::dup2(readPipe[1], STDOUT_FILENO);
        ::close(writePipe[1]); writePipe[1] = -1;
        ::close(readPipe[0]); readPipe[0] = -1;
        P_ENSURE(::execvp(args[0], (char *const*)args) != -1);
    }

    ::close(writePipe[0]); writePipe[0] = -1;
    ::close(readPipe[1]); readPipe[1] = -1;

    BlockingIO::writeN(writePipe[1], input, strlen(input));
    ::close(writePipe[1]); writePipe[1] = -1;

    string ret;
    char buf[512];
    int n;
    while ((n = BlockingIO::readSome(readPipe[0], buf, sizeof(buf))) > 0) {
        buf[n] = 0;
        ret += buf;
    }
    return ret;
}

class Logger: public ILogger {
    DISABLE_COPY(Logger);
public:
    Logger(){}
    virtual void log(const char *msg) {
        LockGuard guard(mMutex);

        char buf[32] = "";
        time_t now; 
        time(&now);
        tm _tm;
        strftime(buf, sizeof(buf), "%F %T", localtime_r(&now, &_tm));

        fprintf(stderr, "[%s] %s\n", buf, msg);
        fflush(stderr);
    }
    virtual void logErr(const char *msg) {
        if (isatty(STDOUT_FILENO)) {
            log(format("\x1b[01;40;31m%s\x1b[0m", msg).c_str());
        } else {
            log(msg);
        }
    }
private:
    Mutex mMutex;
};

ILogger* ILogger::instance() {
    static Logger s_logger;
    return &s_logger;
}
