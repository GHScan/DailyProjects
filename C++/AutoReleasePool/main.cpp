#include "pch.h" 

#include <stdint.h>
#include <math.h>
#include <string.h>
#include <time.h>
#include <assert.h>

#include <string>
#include <vector>
#include <iostream>
using namespace std;

class NSObject {
public:
    NSObject(const string& name): m_ref(1), m_name(name){}
    virtual ~NSObject() {
        printf("destructd : %s\n", m_name.c_str());
    }
    void retain() { ++m_ref;}
    void release() {
        if (--m_ref == 0) delete this;
    }
    NSObject* autoRelease();
private:
    int m_ref;
    string m_name;
};

class AutoReleasePool: public NSObject {
public:
    static AutoReleasePool* getTopPool() {
        return s_poolStack.back();
    }

    AutoReleasePool(const string& name): NSObject(name) {
        s_poolStack.push_back(this);
    }
    ~AutoReleasePool() {
        for (int i = 0; i < (int)m_objs.size(); ++i) {
            m_objs[i]->release();
        }

        assert(s_poolStack.back() == this);
        s_poolStack.pop_back();
    }
    void addObj(NSObject* obj) {
        m_objs.push_back(obj);
    }

private:
    static vector<AutoReleasePool*> s_poolStack;
    vector<NSObject*> m_objs;
};
vector<AutoReleasePool*> AutoReleasePool::s_poolStack;

NSObject* NSObject::autoRelease() {
    AutoReleasePool::getTopPool()->addObj(this);
    return this;
}

int main() {
    {
        puts("============= test 1:");

        NSObject *a = new NSObject("obj1");
        a->release();
    }
    {
        puts("============= test 2:");

        NSObject *a = new NSObject("obj2");
        a->retain();
        a->release();
        a->release();
    }
    {
        puts("============= test 3:");

        AutoReleasePool *pool = new AutoReleasePool("pool3");

        NSObject *a = (new NSObject("obj3.1"))->autoRelease();
        (void)a;

        NSObject *b = (new NSObject("obj3.2"))->autoRelease();
        // for further acccess
        b->retain();

        pool->release();

        b->release();
    }
    {
        puts("============ test 4:");

        AutoReleasePool *pool = new AutoReleasePool("pool4");

        // here we will create amount of temporary objects which is delegated by
        // autorelease(), but we can't wait for the release of outter
        // autoreleasepool, so we create a new one, then the memory usage will
        // be more real time
        {
            AutoReleasePool *pool = new AutoReleasePool("pool4.1");
            for (int i = 0; i < 5; ++i) (new NSObject("obj4.1"))->autoRelease();
            pool->release();
        }
        {
            AutoReleasePool *pool = new AutoReleasePool("pool4.2");
            for (int i = 0; i < 5; ++i) (new NSObject("obj4.2"))->autoRelease();
            pool->release();
        }

        pool->release();
    }
}
