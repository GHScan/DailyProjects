#include "pch.h"

#include <sys/time.h>
#include <semaphore.h>
#include <pthread.h>
#include <unistd.h>
#include <poll.h>

class Semaphore {
public:
    Semaphore(int value) {
        sem_init(&mSem, 0, value);
    }
    ~Semaphore() {
        sem_destroy(&mSem);
    }
    Semaphore(const Semaphore&) = delete;
    Semaphore& operator = (const Semaphore&) = delete;
    void acquire() { sem_wait(&mSem); }
    void release() { sem_post(&mSem); }
private:
    sem_t mSem;
};
class Mutex: private Semaphore {
public:
    Mutex(): Semaphore(1){}
    void lock() { acquire(); }
    void unlock() { release(); }
private:
};
class PosixMutex {
public:
    PosixMutex() { pthread_mutex_init(&mMutex, NULL); }
    ~PosixMutex() { pthread_mutex_destroy(&mMutex); }
    pthread_mutex_t* getInternal() { return &mMutex;}
    void lock() { pthread_mutex_lock(&mMutex); }
    void unlock() { pthread_mutex_unlock(&mMutex); }
private:
    pthread_mutex_t mMutex;
};
template<typename MutexT>
class MutexGuard {
public:
    MutexGuard(MutexT &mutex): mMutex(mutex){ mMutex.lock(); }
    ~MutexGuard(){ mMutex.unlock(); }
private:
    MutexT &mMutex;
};

class ReaderWriterLock {
public:
    ReaderWriterLock(): mReaderCount(0){}
    ~ReaderWriterLock() { assert(mReaderCount == 0); }
    void lockRead() {
        (MutexGuard<PosixMutex>(mWriterPriorMutex));
        MutexGuard<PosixMutex> guard(mReaderCountMutex);
        if (++mReaderCount == 1) mResMutex.lock();
    }
    void unlockRead() {
        MutexGuard<PosixMutex> guard(mReaderCountMutex);
        if (--mReaderCount == 0) mResMutex.unlock();
    }
    void lockWrite() {
        mWriterPriorMutex.lock();
        mResMutex.lock();
    }
    void unlockWrite() {
        mResMutex.unlock();
        mWriterPriorMutex.unlock();
    }
private:
    int mReaderCount;
    PosixMutex mReaderCountMutex;
    PosixMutex mWriterPriorMutex;
    PosixMutex mResMutex;
};
class ReaderWriterLock2 {
public:
    ReaderWriterLock2(): mReaderCount(0), mWriterCount(0) {}
    ~ReaderWriterLock2() { assert(mReaderCount == 0 && mWriterCount == 0); }
    void lockRead() {
        {
            MutexGuard<PosixMutex> guard(mReaderExcludeReaderMutex);
            (MutexGuard<PosixMutex>(mWriterExcludeReaderMutex));
        }
        {
            MutexGuard<PosixMutex> guard(mReaderCountMutex);
            if (++mReaderCount == 1) mResMutex.lock();
        }
    }
    void unlockRead() {
        {
            MutexGuard<PosixMutex> guard(mReaderCountMutex);
            if (--mReaderCount == 0) mResMutex.unlock();
        }
    }
    void lockWrite() {
        {
            MutexGuard<PosixMutex> guard(mWriterCountMutex);
            if (++mWriterCount == 1) mWriterExcludeReaderMutex.lock();
        }
        mResMutex.lock();
    }
    void unlockWrite() {
        mResMutex.unlock();
        {
            MutexGuard<PosixMutex> guard(mWriterCountMutex);
            if (--mWriterCount == 0) mWriterExcludeReaderMutex.unlock();
        }
    }
private:
    int mReaderCount;
    PosixMutex mReaderCountMutex;
    PosixMutex mReaderExcludeReaderMutex;
    int mWriterCount;
    PosixMutex mWriterCountMutex;
    PosixMutex mWriterExcludeReaderMutex;
    PosixMutex mResMutex;
};

class PosixReaderWriterLock {
public:
    PosixReaderWriterLock() {
        pthread_rwlock_init(&mLock, NULL);
    }
    ~PosixReaderWriterLock() {
        pthread_rwlock_destroy(&mLock);
    }
    void lockRead() {
        pthread_rwlock_rdlock(&mLock);
    }
    void unlockRead() {
        pthread_rwlock_unlock(&mLock);
    }
    void lockWrite() {
        pthread_rwlock_wrlock(&mLock);
    }
    void unlockWrite() {
        pthread_rwlock_unlock(&mLock);
    }
private:
    pthread_rwlock_t mLock;
};

template<typename RWLockT>
class ReaderGuard {
public:
    ReaderGuard(RWLockT &lock): mLock(lock){ mLock.lockRead(); }
    ~ReaderGuard(){ mLock.unlockRead(); }
private:
    RWLockT &mLock;
};
template<typename RWLockT>
class WriterGuard {
public:
    WriterGuard(RWLockT& lock): mLock(lock){ mLock.lockWrite(); }
    ~WriterGuard(){ mLock.unlockWrite(); }
private:
    RWLockT &mLock;
};

class PosixWaiter {
public:
    PosixWaiter(): mSignaled(false) { pthread_cond_init(&mCond, NULL); }
    ~PosixWaiter() { pthread_cond_destroy(&mCond); }
    void wait() {
        MutexGuard<PosixMutex> guard(mMutex);
        while (!mSignaled) pthread_cond_wait(&mCond, mMutex.getInternal());
    }
    void signal() {
        MutexGuard<PosixMutex> guard(mMutex);
        mSignaled = true;
        pthread_cond_broadcast(&mCond);
    }
private:
    bool mSignaled;
    PosixMutex mMutex;
    pthread_cond_t mCond;
};

#define _CONN(a, b) a##b
#define CONN(a, b) _CONN(a, b)
#define _TO_STRING(s) #s
#define TO_STRING(s) _TO_STRING(s)
//////////////////////////////
struct ThreadData {
    function<void()> f;
};
static void* thread(void* _data) {
    pthread_detach(pthread_self());
    ((ThreadData*)_data)->f();
    return NULL;
}
static void createThread(function<void()> f) {
    pthread_attr_t attri;
    pthread_attr_init(&attri);
    pthread_attr_setstacksize(&attri, 1 << 14);

    pthread_t tid;
    pthread_create(&tid, &attri, &thread, new ThreadData{f});
}

#define CONCURRENT 300
#define READER_LOOP 5000
#define WRITER_LOOP 100
#define WRITER_COUNT 3
#define READER_COUNT (WRITER_COUNT * CONCURRENT)

#define LOCK_TYPE_NULL 50
#define LOCK_TYPE_RW 10
#define LOCK_TYPE_RW2 11
#define LOCK_TYPE_POSIX_RW 20
#define LOCK_TYPE_MUTEX 30
#define LOCK_TYPE_POSIX_MUTEX 40
#define TYPE RW
#define TYPE_NAME TO_STRING(TYPE)
#define USE_LOCK CONN(LOCK_TYPE_, TYPE)

ReaderWriterLock g_resRW;
ReaderWriterLock2 g_resRW2;
PosixReaderWriterLock g_posixResRW;
Mutex g_resMutex;
PosixMutex g_posixResMutex;
int g_res;
int g_res2;

Mutex g_gMutex;
double g_readerTime;
double g_writerTime;

PosixWaiter g_threadsWaiter;
Semaphore g_mainWaiter(0);

static string doRead() {
    char buf[32];
    for (int i = 0; i < 10; ++i) {
        sprintf(buf, "%d", g_res2 - g_res);
    }
    return buf;
}
static string doWrite() {
    char buf[32];
    for (int i = 0; i < 100; ++i) {
        sprintf(buf, "%d", g_res + 1);
        sscanf(buf, "%d", &g_res2);
        g_res = g_res2;
    }
    return buf;
}

static double getTime() {
    timeval tv;
    gettimeofday(&tv, NULL);
    return tv.tv_sec + double(tv.tv_usec) / 1000000;
}
static void sleep_us(int us) {
    poll(NULL, 0, us);
}

int main() {
    for (int i = 0; i < READER_COUNT; ++i) {
        createThread([&](){
                g_mainWaiter.release();
                g_threadsWaiter.wait();

                double start = getTime();
                for (int j = 0; j < READER_LOOP; ++j) {
                    {
#if USE_LOCK == LOCK_TYPE_RW
                        ReaderGuard<ReaderWriterLock> guard(g_resRW);
#elif USE_LOCK == LOCK_TYPE_RW2
                        ReaderGuard<ReaderWriterLock2> guard(g_resRW2);
#elif USE_LOCK == LOCK_TYPE_POSIX_RW
                        ReaderGuard<PosixReaderWriterLock> guard(g_posixResRW);
#elif USE_LOCK == LOCK_TYPE_MUTEX
                        MutexGuard<Mutex> guard(g_resMutex);
#elif USE_LOCK == LOCK_TYPE_POSIX_MUTEX
                        MutexGuard<PosixMutex> guard(g_posixResMutex);
#else
#endif
                        doRead();
                    }
                }
                double readerTime = getTime() - start;

                MutexGuard<Mutex> guard(g_gMutex);
                g_readerTime += readerTime;
                g_mainWaiter.release();
                });
    }
    for (int i = 0; i < WRITER_COUNT; ++i) {
        createThread([&](){
                g_mainWaiter.release();
                g_threadsWaiter.wait();

                double start = getTime();
                for (int j = 0; j < WRITER_LOOP; ++j) {
                    sleep_us(10);
                    {
#if USE_LOCK == LOCK_TYPE_RW
                        WriterGuard<ReaderWriterLock> guard(g_resRW);
#elif USE_LOCK == LOCK_TYPE_RW2
                        WriterGuard<ReaderWriterLock2> guard(g_resRW2);
#elif USE_LOCK == LOCK_TYPE_POSIX_RW
                        WriterGuard<PosixReaderWriterLock> guard(g_posixResRW);
#elif USE_LOCK == LOCK_TYPE_MUTEX
                        MutexGuard<Mutex> guard(g_resMutex);
#elif USE_LOCK == LOCK_TYPE_POSIX_MUTEX
                        MutexGuard<PosixMutex> guard(g_posixResMutex);
#else
#endif
                        doWrite();
                    }
                }
                double writerTime = getTime() - start;

                MutexGuard<Mutex> guard(g_gMutex);
                g_writerTime += writerTime;
                g_mainWaiter.release();
                });
    }

    for (int i = 0; i < READER_COUNT + WRITER_COUNT; ++i) g_mainWaiter.acquire();
    g_threadsWaiter.signal();
    for (int i = 0; i < READER_COUNT + WRITER_COUNT; ++i) g_mainWaiter.acquire();

    fprintf(stderr, "res=%d,res2=%d=\n", g_res, g_res2);
    printf("lockType=%s, R/W=%d, reader=%.3fs, writer=%.3fs\n", TYPE_NAME, READER_COUNT / WRITER_COUNT, g_readerTime / READER_COUNT, g_writerTime / WRITER_COUNT);
}
