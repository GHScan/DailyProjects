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
    void lockRead() {
        (MutexGuard<Mutex>(mWriterPriorMutex));
        MutexGuard<Mutex> guard(mReaderCountMutex);
        if (++mReaderCount == 1) mResMutex.lock();
    }
    void unlockRead() {
        MutexGuard<Mutex> guard(mReaderCountMutex);
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
    Mutex mReaderCountMutex;
    Mutex mWriterPriorMutex;
    Mutex mResMutex;
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
#define READER_LOOP 1000
#define WRITER_LOOP 100
#define WRITER_COUNT 3
#define READER_COUNT (WRITER_COUNT * CONCURRENT)

#define LOCK_TYPE_RW 1
#define LOCK_TYPE_POSIX_RW 2
#define LOCK_TYPE_MUTEX 3
#define LOCK_TYPE_POSIX_MUTEX 4
#define LOCK_TYPE_NULL 5
#define USE_LOCK LOCK_TYPE_POSIX_MUTEX

Mutex g_gMutex;
int g_writerCount = WRITER_COUNT;
int g_readerCount = READER_COUNT;

ReaderWriterLock g_resRW;
PosixReaderWriterLock g_posixResRW;
Mutex g_resMutex;
PosixMutex g_posixResMutex;
int g_res;
int g_res2;

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
    for (int i = 0; i < g_readerCount; ++i) {
        createThread([&](){
                sleep_us(100);
                double start = getTime();
                for (int j = 0; j < READER_LOOP; ++j) {
                    {
#if USE_LOCK == LOCK_TYPE_RW
                        ReaderGuard<ReaderWriterLock> guard(g_resRW);
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

                MutexGuard<Mutex> guard(g_gMutex);
                if (--g_readerCount == 0) {
                    printf("(%.3fsec), RW ratio=%d, all reader exit\n", getTime() - start, READER_COUNT / WRITER_COUNT);
                }
                });
    }
    for (int i = 0; i < g_writerCount; ++i) {
        createThread([&](){
                sleep_us(100);
                double start = getTime();
                for (int j = 0; j < WRITER_LOOP; ++j) {
                    sleep_us(10);
                    {
#if USE_LOCK == LOCK_TYPE_RW
                        WriterGuard<ReaderWriterLock> guard(g_resRW);
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

                MutexGuard<Mutex> guard(g_gMutex);
                if (--g_writerCount == 0) {
                    printf("(%.3fsec), RW ratio=%d, all writer exit, g_res=%d,g_res2=%d\n", getTime() - start, READER_COUNT / WRITER_COUNT, g_res, g_res2);
                }
                });
    }

    pause();
}
