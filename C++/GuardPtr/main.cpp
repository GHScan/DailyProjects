#include "pch.h"

template<typename T, typename LockT>
class GuardPtr {
public:
    struct GuardCall {
    public:
        GuardCall(T* ptr, LockT *lock): mPtr(ptr), mLock(lock) { mLock->lock(); }
        ~GuardCall() { mLock->unlock();}
        T* operator -> () { return mPtr; }
    private:
        T *mPtr;
        LockT *mLock;
    };
public:
    GuardPtr(T *ptr, LockT *lock): mPtr(ptr), mLock(lock){}
    T* get() { return mPtr; }
    GuardCall operator -> () {
        return GuardCall(mPtr, mLock);
    }
private:
    T *mPtr;
    LockT *mLock;
};

class Mutex {
public:
    Mutex(){}
    void lock() { printf("Mutex(%p).lock\n", this); }
    void unlock() { printf("Mutex(%p).unlock\n", this); }
    Mutex(const Mutex&);
    Mutex& operator = (const Mutex&);
};
class A {
public:
    void set(int) { printf("A(%p).set\n", this); }
    int get() const {
        printf("A(%p).get\n", this);
        return 1;
    }
};

int main() {
    Mutex m;
    GuardPtr<A, Mutex> p(new A, &m);

    p->get();
    p->set(1);

    delete p.get();
}
