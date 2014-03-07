#ifndef THREADIING_H
#define THREADIING_H

#include <functional>
#include <queue>

#include <pthread.h>
#include <semaphore.h>

#include "Utils.h"
//////////////////////////////
class Mutex {
    DISABLE_COPY(Mutex);
public:
    Mutex();
    ~Mutex();
    void lock();
    void unlock();
    pthread_mutex_t* getInternal() { return &mMutex;}
private:
    pthread_mutex_t mMutex;
};

class CondVar {
    DISABLE_COPY(CondVar);
public:
    CondVar();
    ~CondVar();
    void signal();
    void broadcast();
    void wait(Mutex *mutex);
private:
    pthread_cond_t mCond;
};

class Semaphore {
    DISABLE_COPY(Semaphore);
public:
    Semaphore(int value);
    ~Semaphore();
    void wait();
    void post();
private:
    sem_t mSem;
};

class RWLock {
    DISABLE_COPY(RWLock);
public:
    RWLock();
    ~RWLock();
    void rlock();
    void wlock();
    void unlock();
private:
    pthread_rwlock_t mRWLock;
};

class LockGuard {
    DISABLE_COPY(LockGuard);
public:
    LockGuard(Mutex &lock): mLock(lock){ mLock.lock(); }
    ~LockGuard() { mLock.unlock(); }
private:
    Mutex &mLock;
};
class RLockGuard {
    DISABLE_COPY(RLockGuard);
public:
    RLockGuard(RWLock &lock): mLock(lock){ mLock.rlock(); }
    ~RLockGuard() { mLock.unlock(); }
private:
    RWLock &mLock;
};
class WLockGuard {
    DISABLE_COPY(WLockGuard);
public:
    WLockGuard(RWLock &lock): mLock(lock){ mLock.wlock(); }
    ~WLockGuard() { mLock.unlock(); }
private:
    RWLock &mLock;
};
//////////////////////////////
class Atomic {
    DISABLE_COPY(Atomic);
public:
    Atomic(int val): mValue(val){}
    int get() const { return mValue; }
    int add(int dv) {
        return __sync_fetch_and_add(&mValue, dv);
    }
    bool cas(int ov, int nv) {
        return __sync_bool_compare_and_swap(&mValue, ov, nv);
    }
private:
    int mValue;
};

//////////////////////////////

class Thread {
    ENABLE_COPY(Thread);
public:
    Thread(){}

    static Thread create(function<void()> f, int stackSize = 0);
    static Thread current();
    static void sleep(int us);

    void join();
    void canel();

    void detach();

    pthread_t getTid() const { return mTid; }
    void setCpuAffinity(int mask);
private:
    struct ThreadCtx {
        function<void()> f;
    };
private:
    Thread(pthread_t tid);
    static void* threadProc(void *p);
private:
    pthread_t mTid;
};
//////////////////////////////
class Waiter {
    DISABLE_COPY(Waiter);
public:
    Waiter();
    void wait();
    void signal();
    void resetSignal();
private:
    bool mSignaled;
    Mutex mMutex;
    CondVar mCond;
};

template<typename T>
class ConcurrentQueue {
    DISABLE_COPY(ConcurrentQueue);
public:
    ConcurrentQueue(int size): mSem(size){}
    ~ConcurrentQueue(){}
    bool size() {
        LockGuard guard(mMutex);
        return mQueue.size();
    }
    T pop() {
        mSem.wait();
        LockGuard guard(mMutex);
        return mQueue.pop();
    }
    void push(const T& val) {
        {
            LockGuard guard(mMutex);
            mQueue.push(val);
        }
        mSem.post();
    }
private:
    Mutex mMutex;
    Semaphore mSem;
    queue<T> mQueue;
};

#endif
