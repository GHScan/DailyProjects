#ifndef SHAREDPTR_H
#define SHAREDPTR_H

#include <algorithm>

namespace SharedPtrImpl
{
    template<typename T>
    void fdelete(T* p)
    {
        delete p;
    }

    template<typename T>
    struct PtrHolder
    {
        T *p;
        void (*fdel)(T*);
        int ref;
        int weakRef;

        PtrHolder(T* _p, void (*_fdel)(T*)):
            p(_p), fdel(_fdel), ref(0), weakRef(0){}

        void addRef()
        {
            addRefWeak();
            ++ref;
        }
        void release()
        {
            if (--ref == 0) {
                fdel(p);
                p = NULL;
            }
            releaseWeak();
        }
        void addRefWeak()
        {
            ++weakRef;
        }
        void releaseWeak()
        {
            if (--weakRef == 0) {
                delete this;
            }
        }
    };
}


template<typename T>
class WeakPtr;

template<typename T>
class SharedPtr
{
public:
    explicit SharedPtr(T* p = NULL):
        m_holder(NULL), m_dp(NULL)
    {
        if (p != NULL) {
            m_holder = new SharedPtrImpl::PtrHolder<T>(p, SharedPtrImpl::fdelete<T>);
            m_holder->addRef();
            m_dp = p;
        }
    }
    SharedPtr(T* p, void (*fdel)(T*)):
        m_holder(NULL), m_dp(NULL)
    {
        if (p != NULL) {
            m_holder = new SharedPtrImpl::PtrHolder<T>(p, fdel);
            m_holder->addRef();
            m_dp = p;
        }
    }

    ~SharedPtr()
    {
        if (m_holder != NULL) m_holder->release();
        m_holder = NULL;
        m_dp = NULL;
    }

    SharedPtr(const SharedPtr& o):
        m_holder(NULL), m_dp(NULL)
    {
        *this = o;
    }
    template<typename U>
    SharedPtr(const SharedPtr<U>& o):
        m_holder(NULL), m_dp(NULL)
    {
        *this = o;
    }

    SharedPtr& operator = (const SharedPtr& o)
    {
        if (this == &o) return *this;
        if (m_holder != NULL) m_holder->release();
        m_holder = o.m_holder;
        if (m_holder != NULL) m_holder->addRef();
        m_dp = o.m_dp;
        return *this;
    }
    template<typename U>
    SharedPtr<T>& operator = (const SharedPtr<U>& o)
    {
        if (m_holder != NULL) m_holder->release();
        m_holder = reinterpret_cast<SharedPtrImpl::PtrHolder<T>*>(o.m_holder);
        if (m_holder != NULL) m_holder->addRef();
        m_dp = o.m_dp;
        return *this;
    }

    template<typename U>
    SharedPtr<U> dynamicCast()
    {
        SharedPtr<U> r;
        r.m_dp = dynamic_cast<U*>(m_dp);
        if (r.m_dp != NULL) {
            r.m_holder = reinterpret_cast<SharedPtrImpl::PtrHolder<U>*>(m_holder);
            r.m_holder->addRef();
        }
        return r;
    }

    T* operator -> ()
    {
        return m_dp;
    }
    T& operator * ()
    {
        return *m_dp;
    }

    operator T* ()
    {
        return m_dp;
    }
    operator const T* () const
    {
        return m_dp;
    }

    int refCount() const
    {
        return m_holder == NULL ? 0 : m_holder->ref;
    }

    void swap(SharedPtr& o)
    {
        std::swap(m_dp, o.m_dp);
        std::swap(m_holder, o.m_holder);
    }

private:
    friend WeakPtr<T>;
    template<typename U>
    friend class SharedPtr;

private:
    SharedPtrImpl::PtrHolder<T> *m_holder;
    T *m_dp;
};

template<typename T>
class WeakPtr
{
public:
    WeakPtr():
        m_holder(NULL)
    { }

    ~WeakPtr()
    {
        if (m_holder != NULL) m_holder->releaseWeak();
        m_holder = NULL;
    }
    
    WeakPtr(const WeakPtr&o):
        m_holder(NULL)
    {
        *this = o;
    }
    WeakPtr(const SharedPtr<T>& o):
        m_holder(NULL)
    {
        *this = o;
    }

    WeakPtr& operator = (const WeakPtr& o)
    {
        if (this == &o) return *this;
        if (m_holder != NULL) m_holder->releaseWeak();
        m_holder = o.m_holder;
        if (m_holder != NULL) m_holder->addRefWeak();
        return *this;
    }
    WeakPtr& operator = (const SharedPtr<T>& o)
    {
        if (m_holder != NULL) m_holder->releaseWeak();
        m_holder = o.m_holder;
        if (m_holder != NULL) m_holder->addRefWeak();
        return *this;
    }

    bool isValid() const
    {
        return m_holder != NULL && m_holder->p != NULL;
    }

    SharedPtr<T> lock()
    {
        SharedPtr<T> r;
        if (!isValid()) return r;
        r.m_holder = m_holder;
        r.m_holder->addRef();
        r.m_dp = m_holder->p;
        return r;
    }

    int weakRefCount() const
    {
        return m_holder == NULL ? 0 : m_holder->weakRef;
    }

    void swap(WeakPtr& o)
    {
        std::swap(m_holder, o.m_holder);
    }

private:
    SharedPtrImpl::PtrHolder<T> *m_holder;
};

namespace std
{
    template<typename T>
    void swap(SharedPtr<T>& a, SharedPtr<T>& b)
    {
        a.swap(b);
    }

    template<typename T>
    void swap(WeakPtr<T>& a, WeakPtr<T>& b)
    {
        a.swap(b);
    }
}

#endif
