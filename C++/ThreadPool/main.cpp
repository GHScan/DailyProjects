#include "pch.h"

#include <unistd.h>
#include <pthread.h>
#include <semaphore.h>

#include <functional>
#include <exception>
#include <queue>
#include <memory>

#define error_check(err) do{if (err != 0) fprintf(stderr, "%s,%d: %s\n", __FILE__, __LINE__, strerror(err)); } while(0)

class Semaphore {
public:
    Semaphore(int value) { sem_init(&mObj, 0, value); }
    ~Semaphore() { sem_destroy(&mObj); }
    void acquire() { sem_wait(&mObj); }
    bool tryAcquire(int timeoutsec) {
        timespec time;
        clock_gettime(CLOCK_REALTIME, &time);
        time.tv_sec += timeoutsec;

        if ((sem_timedwait(&mObj, &time)) != 0) {
            if (errno != ETIMEDOUT) error_check(errno);
            return false;
        }
        return true;
    }
    void release() { sem_post(&mObj); }
private:
    Semaphore(const Semaphore&) = delete;
    Semaphore& operator = (const Semaphore&) = delete;
private:
    sem_t mObj;
};
class Mutex: private Semaphore {
public:
    Mutex(): Semaphore(1){}
    void lock() { acquire(); }
    void unlock() { release(); }
};
class CondVar: private Semaphore {
public:
    CondVar(): Semaphore(0){}
    void notifyOne() { release(); }
    void wait() { acquire(); }
};
class MutexGuard {
public:
    MutexGuard(Mutex &m): mMutex(m) { mMutex.lock(); }
    ~MutexGuard() { mMutex.unlock(); }
private:
    Mutex &mMutex;
};

class ThreadPool {
public:
    class MultiTaskResult {
    public:
        void join() {
            mAllComplete.wait();
        }
    private:
        friend class ThreadPool;
        MultiTaskResult(int taskCount): mTaskCount(taskCount){ assert(mTaskCount > 0); }
        void onTaskComplete() {
            if (__sync_fetch_and_add(&mTaskCount, -1) == 1) {
                mAllComplete.notifyOne();
            }
        }
    private:
        CondVar mAllComplete;
        int mTaskCount;
    };
public:
    ThreadPool(int threadCnt): mTaskCount(0), mPoolExit(false) {
        for (int i = 0; i < threadCnt; ++i) {
            pthread_t tid;
            int err = pthread_create(&tid, nullptr, &_threadRoutine, new ThreadData{this, i});
            error_check(err);
            mThreads.push_back(tid);
        }
    }
    ~ThreadPool() {
        while (hasTask()) {
            function<void()> f = popTask(0);
            if (f) safeCall(f);
        }

        mPoolExit = true;
        for (auto tid : mThreads) {
            pthread_join(tid, nullptr);
        }
    }
    
    template<typename FuncT, typename IterT>
    shared_ptr<MultiTaskResult> apply(FuncT f, IterT begin, IterT end) {
        vector<function<void()>> funcs;
        for (; begin != end; ++begin) {
            auto v = *begin;
            funcs.push_back([f, v](){ f(v); });
        }
        return postmulti(funcs.begin(), funcs.end());
    }
    template<typename FuncIter>
    shared_ptr<MultiTaskResult> postmulti(FuncIter begin, FuncIter end) { 
        auto result = shared_ptr<MultiTaskResult>(new MultiTaskResult(distance(begin, end)));

        for (; begin != end; ++begin) {
            auto f = *begin;
            pushTask([f, result](){
                    safeCall(f);
                    result->onTaskComplete();
                    });
        }

        return result;
    }
    void post(function<void()> f) {
        pushTask(f);
    }
private:
    static void* _threadRoutine(void *_p) {
        auto p = (ThreadData*)_p;
        ThreadPool *pool = p->pool;
        int threadIdx = p->threadIdx;
        delete p;

        return pool->threadRoutine(threadIdx);
    }
    void* threadRoutine(int threadIdx) {
        int err;
        pthread_t tid = pthread_self();

        {
            int cpuCnt = sysconf(_SC_NPROCESSORS_ONLN);
            cpu_set_t cpuset;
            CPU_ZERO(&cpuset);
            CPU_SET(threadIdx % cpuCnt, &cpuset);
            err = pthread_setaffinity_np(tid, sizeof(cpuset), &cpuset);
            error_check(err);
        }

        while (!mPoolExit) {
            if (function<void()> f = popTask(1)) {
                safeCall(f);
            }
        }

        return nullptr;
    }
    function<void()> popTask(int timeoutsec) {
        function<void()> f;
        if (!mTaskCount.tryAcquire(timeoutsec)) return f;

        {
            MutexGuard guard(mThisLocker);
            f = mTaskQueue.front();
            mTaskQueue.pop();
            return f;
        }
    }
    void pushTask(function<void()> f) {
        {
            MutexGuard guard(mThisLocker);
            mTaskQueue.push(f);
        }
        mTaskCount.release();
    }
    bool hasTask() {
        MutexGuard guard(mThisLocker);
        return !mTaskQueue.empty();
    }
    static void safeCall(function<void()> f) {
        try {
            f();
        } catch(const exception& e) {
            fprintf(stderr, "%s,%d: uncaught exception - %s\n", __FILE__, __LINE__, e.what());
        }
    }
private:
    struct ThreadData {
        ThreadPool *pool;
        int threadIdx;
    };
private:
    Mutex mThisLocker;
    vector<pthread_t> mThreads;
    queue<function<void()>> mTaskQueue;
    Semaphore mTaskCount;
    bool mPoolExit;
};

int main() {
    printf("main pthread=%lx\n", pthread_self());

    ThreadPool p(4);

    function<void(int)> f = [](int i){  
        sleep(1);
        printf("pid=%lx,id=%d\n", pthread_self(), i);
    };
    vector<int> a = {1, 2, 3, 4, 5, 6, 7, 8};

    p.apply(f, a.begin(), a.end())->join();
    puts("join finished!");
    for (auto i : a) cout << i << ','; puts("");

    p.apply(f, a.begin(), a.end());

    puts("exit main");
}
