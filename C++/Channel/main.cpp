#include "pch.h"

#include <functional>

#include <pthread.h>
#include <semaphore.h>

class Semaphore {
public:
    Semaphore(int value) { ::sem_init(&mSem, 0, value); }
    ~Semaphore() { ::sem_destroy(&mSem); }
    void wait() { ::sem_wait(&mSem); }
    void signal() { ::sem_post(&mSem); }
private:
    sem_t mSem;
};
class Mutex {
public:
    Mutex() { ::pthread_mutex_init(&mMutex, nullptr); }
    ~Mutex() { ::pthread_mutex_destroy(&mMutex); }
    void lock() { ::pthread_mutex_lock(&mMutex); }
    void unlock() { ::pthread_mutex_unlock(&mMutex); }
    pthread_mutex_t *getInternal() { return &mMutex; }
private:
    pthread_mutex_t mMutex;
};
class CondVar {
public:
    CondVar(): mSignaled(0) { ::pthread_cond_init(&mCond, nullptr); }
    ~CondVar() { ::pthread_cond_destroy(&mCond); }
    void wait(Mutex *mutex) {
        while (!mSignaled || !__sync_bool_compare_and_swap(&mSignaled, 1, 0)) {
            ::pthread_cond_wait(&mCond, mutex->getInternal());
        }
    }
    void signal() {
        mSignaled = 1;
        ::pthread_cond_signal(&mCond);
    }
private:
    pthread_cond_t mCond;
    int mSignaled;
};
//////////////////////////////
class Channel {
public:
    int send(const void *p, int size) {
        mMutex.lock();

        mPtr = p;
        mSize = &size;
        mSent.signal();

        mReceived.wait(&mMutex);

        mMutex.unlock();
        return size;
    }
    int receive(void *p, int size) {
        mMutex.lock();

        mSent.wait(&mMutex);

        size = min(size, *mSize);
        if (size > 0) memcpy(p, mPtr, size);
        *mSize = size;
        mReceived.signal();

        mMutex.unlock();
        return size;
    }
    template<typename T>
    bool send(const T &v) {
        return send(&v, sizeof(v)) == sizeof(v);
    }
    template<typename T>
    bool receive(T &v) {
        return receive(&v, sizeof(v)) == sizeof(v);
    }
private:
    Mutex mMutex;
    CondVar mSent;
    CondVar mReceived;
    const void *mPtr;
    int *mSize;
};

class Channel2 {
public:
    Channel2(): mSent(0), mReceived(0) { }
    int send(const void *p, int size) {
        mSendable.lock();

        mPtr = p;
        mSize = &size;
        mSent.signal();

        mReceived.wait();

        mSendable.unlock();
        return size;
    }
    int receive(void *p, int size) {
        mSent.wait();

        size = min(size, *mSize);
        if (size > 0) memcpy(p, mPtr, size);
        *mSize = size;

        mReceived.signal();
        return size;
    }
    template<typename T>
    bool send(const T &v) {
        return send(&v, sizeof(v)) == sizeof(v);
    }
    template<typename T>
    bool receive(T &v) {
        return receive(&v, sizeof(v)) == sizeof(v);
    }
private:
    Mutex mSendable;
    Semaphore mSent;
    Semaphore mReceived;
    const void *mPtr;
    int *mSize;
};

//////////////////////////////
struct ThreadArgs {
    function<void()> f;
};
static void* threadProc(void *_args) {
    auto p = static_cast<ThreadArgs*>(_args);
    p->f();
    delete p;
    return nullptr;
}
static void createThread(function<void()> f) {
    pthread_attr_t attr;
    ::pthread_attr_init(&attr);
    ::pthread_attr_setstacksize(&attr, 16 * 1024);
    pthread_t tid;
    int err = ::pthread_create(&tid, &attr, &threadProc, new ThreadArgs{f});
    if (err) perror("create thread failed!\n");
}


template<typename ChannelT>
static void filter(ChannelT *from, int prime, int max) {
#ifndef NDEBUG
    if (prime <= max) printf("%d,", prime);
#endif
    if (prime >= max) {
        puts("");
        from->receive(nullptr, 0);
        return;
    }

    int next;
    for (; from->receive(next) && next % prime == 0;);
    ChannelT *to = new ChannelT();
    createThread([=](){ filter(to, next, max); });

    for (int i; from->receive(i); ) {
        if (i % prime == 0) continue;
        if (!to->send(i)) break;
    }

    from->receive(nullptr, 0);
    delete to;
}

template<typename ChannelT>
void primeGen(int max) {
    Semaphore sem(0);

    createThread([max, &sem](){
        ChannelT *to = new ChannelT();
        createThread([to, max](){filter(to, 2, max);});

        for (int i = 2; to->send(i); ++i);
        delete to;

        sem.signal();
    });

    sem.wait();
}
//////////////////////////////
#define TIMEIT(...) do { clock_t start = clock(); __VA_ARGS__; printf("%f\n", float(clock() - start) / CLOCKS_PER_SEC);} while(0)

int main() {
    const int PRIMES = 3000;
    TIMEIT(primeGen<Channel>(PRIMES));
    TIMEIT(primeGen<Channel2>(PRIMES));
}
