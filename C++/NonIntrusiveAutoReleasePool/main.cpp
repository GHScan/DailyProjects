#include "pch.h"

#include <assert.h>
#include <stdarg.h>

#include <utility>
#include <vector>
#include <string>

class AutoReleasePool {
public:
    AutoReleasePool(): mNext(s_top) {
        s_top = this;
    }
    ~AutoReleasePool() {
        assert(s_top == this);
        s_top = this->mNext;
        for (int i = 0; i < (int)mDeleters.size(); ++i) {
            mDeleters[i].second(mDeleters[i].first);
        }
    }
    static AutoReleasePool* top() {
        return s_top;
    }

    template<typename T>
    T* addObj(T *p) {
        mDeleters.push_back(make_pair(p, &delObj<T>));
        return p;
    }
    template<typename T>
    T* addArray(T *p) {
        mDeleters.push_back(make_pair(p, &delArray<T>));
        return p;
    }
    void* addPtr(void *p) {
        mDeleters.push_back(make_pair(p, &free));
        return p;
    }
private:
    typedef void (*DelFuncT)(void*);
    template<typename T>
    static void delObj(void *p) {
        delete (T*)p;
    }
    template<typename T>
    static void delArray(void *p) {
        delete[] (T*)p;
    }
private:
    AutoReleasePool(const AutoReleasePool&);
    AutoReleasePool& operator = (const AutoReleasePool&);
private:
    std::vector<std::pair<void*, DelFuncT> > mDeleters;
    AutoReleasePool* mNext;
private:
    static AutoReleasePool *s_top;
};
AutoReleasePool* AutoReleasePool::s_top;
//////////////////////////////
class Type1 {
public:
    Type1(const char *name): mName(name){}
    virtual ~Type1(){ printf("Type1 destruct: %p,%s\n", this, mName); }
protected:
    const char *mName;
};
class Type2: public Type1 {
public:
    Type2(const char *name): Type1(name){}
    virtual ~Type2(){ printf("Type2-"); }
};

static char* format(const char *fmt, ...) {
    static char s_buf[512];
    va_list args;
    va_start(args, fmt);
    vsprintf(s_buf, fmt, args);
    va_end(args);
    return s_buf;
}

int main() {
    AutoReleasePool *gPool = new AutoReleasePool();

    int *pi = new int(5);
    std::string *ps = new std::string("abcd");
    AutoReleasePool::top()->addObj(pi);
    AutoReleasePool::top()->addObj(ps)->size();
    AutoReleasePool::top()->addPtr(malloc(5));

    for (int eventLoop = 0; eventLoop < 5; ++eventLoop) {
        printf("@ event loop %d\n", eventLoop);

        AutoReleasePool loopPool;

        Type1 *obj = eventLoop % 2 ? 
                new Type1(format("loop=%d", eventLoop)) : new Type2(format("loop=%d", eventLoop));
        AutoReleasePool::top()->addObj(obj);

        AutoReleasePool::top()->addArray(new string[3]);
    }

    delete gPool;
}
