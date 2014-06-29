#ifndef SINGLETON_H
#define SINGLETON_H

template<typename T>
class Singleton {
public:
    Singleton() {
        ASSERT(sInstance == nullptr);
        sInstance = static_cast<T*>(this);
    }

    ~Singleton() {
        ASSERT(sInstance == this);
        sInstance = nullptr;
    }

    static T* instance() {
        return sInstance;
    }

    Singleton(const Singleton&) = delete;
    Singleton& operator = (const Singleton&) = delete;

private:
    static T* sInstance;
};

template<typename T>
T* Singleton<T>::sInstance = nullptr;

#endif
