#ifndef UTILS_H
#define UTILS_H

#include <exception>
#include <utility>
#include <functional>
#include <sstream>
//////////////////////////////
extern string format(const char *fmt, ...);

#define _TO_STRING(a) #a
#define TO_STRING(a) _TO_STRING(a)
#define _CONN(a, b) a##b
#define CONN(a, b) _CONN(a, b)

#define DISABLE_COPY(type) type(const type&) = delete; type& operator = (const type&) = delete
#define ENABLE_COPY(type) 

#define ARRAY_SIZE(a) (sizeof(a) / sizeof(a[0]))
//////////////////////////////
#define EXCEPTION_CTX_OPERATOR(type) template<typename T>\
    type& operator << (pair<const char*, T> varInfo) {\
        ostringstream os;\
        os << "\t\t" << varInfo.first << ':' << varInfo.second << '\n';\
        mWhat += os.str();\
        return *this;\
    }\
    type& operator << (const char *s) { return *this; }

class Throwable: public exception {
    ENABLE_COPY(Throwable)
public:
    const char* what() const throw() { return mWhat.c_str(); }
protected:
    Throwable(const char *what): mWhat(what){}
protected:
    string mWhat;
};

class LogicError: public Throwable {
    ENABLE_COPY(LogicError)
public:
    LogicError(const char *describ, const char *file, int line);
    EXCEPTION_CTX_OPERATOR(LogicError);
private:
    string constructErrorMsg(const char *describ, const char *file, int line);
    string stackTrace();
};

class RuntimeException: public Throwable {
    ENABLE_COPY(RuntimeException)
protected:
    RuntimeException(const char *what): Throwable(what){}
};

class PosixException: public RuntimeException {
    ENABLE_COPY(PosixException)
public:
    PosixException(int err, const char *describ, const char *file, int line);
    EXCEPTION_CTX_OPERATOR(PosixException);
};

#undef EXCEPTION_CTX_OPERATOR

extern const char *_EXCEPTION_CTX_A;
extern const char *_EXCEPTION_CTX_B;
#define _EXCEPTION_CTX_A(v) make_pair(#v, v) << _EXCEPTION_CTX_B
#define _EXCEPTION_CTX_B(v) make_pair(#v, v) << _EXCEPTION_CTX_A
#define P_ENSURE_ERR(b, err) if (b); else throw PosixException(err, #b, __FILE__, __LINE__) << _EXCEPTION_CTX_A
#define P_ENSURE(b) P_ENSURE_ERR(b, errno)
#define P_ENSURE_R(exp) for (int err = exp; err != 0; err = 0) throw PosixException(err, #exp, __FILE__, __LINE__) << _EXCEPTION_CTX_A
#define ASSERT(b) if (b); else throw LogicError(#b, __FILE__, __LINE__) << _EXCEPTION_CTX_A

//////////////////////////////
class ScopeGuard {
    DISABLE_COPY(ScopeGuard);
public:
    ScopeGuard(function<void()> f): mF(f) {}
    ~ScopeGuard() { if (mF) mF(); }
    void dismiss() { mF = nullptr; }
private:
    function<void()> mF;
};

#define ON_EXIT_SCOPE(f) ScopeGuard CONN(__scopeVar_, __LINE__)(f)
//////////////////////////////
extern string cmdOpenAndRetrieve(const char **args, const char *input);

//////////////////////////////
class ILogger {
    DISABLE_COPY(ILogger);
public:
    ILogger(){}
    static ILogger* instance();
    virtual void log(const char *msg) = 0;
    virtual void logErr(const char *msg) = 0;
};

#ifdef NDEBUG
#define LOG(fmt, ...)
#else
#define LOG(fmt, ...)  ILogger::instance()->log(format("%s(%d): " fmt, __FILE__, __LINE__, __VA_ARGS__).c_str())
#endif

#define LOG_ERR(fmt, ...) ILogger::instance()->logErr(format("%s(%d): " fmt, __FILE__, __LINE__, __VA_ARGS__).c_str())

#endif
