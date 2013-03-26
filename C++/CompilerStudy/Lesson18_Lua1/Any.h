#ifndef ANY_H
#define ANY_H

#include <typeinfo>

class Any {
private:
    struct IHolder {
        const type_info *info;
        IHolder(const type_info* _info): info(_info){}
        virtual ~IHolder(){}
        virtual void* getPtr() = 0;
        virtual IHolder* clone() = 0;
    };
    template<typename T>
    struct Holder:
        public IHolder {
        T value;
        explicit Holder(const T &val): IHolder(&typeid(T)), value(val){}
        virtual void* getPtr(){ return &value;}
        virtual IHolder* clone() { return new Holder(value);}
    };
public:
    Any(): m_holder(NULL) {
    }
    Any(const Any& o): m_holder(NULL) {
        *this = o;
    }
    Any(Any&& o): m_holder(NULL) {
        *this = forward<Any>(o);
    }
    Any& operator = (const Any& o) {
        if (this != &o) {
            this->~Any();
            if (o.m_holder != NULL) m_holder = o.m_holder->clone();
            else m_holder = NULL;
        }
        return *this;
    }
    Any& operator = (Any &&o) {
        if (this != &o) {
            this->~Any();
            m_holder = o.m_holder;
            o.m_holder = NULL;
        }
        return *this;
    }
    ~Any() {
        delete m_holder;
        m_holder = NULL;
    }
    template<typename T>
    Any(const T& v) {
        m_holder = new Holder<T>(v);
    }
    template<typename T>
    Any& operator = (const T &v) {
        delete m_holder;
        m_holder = new Holder<T>(v);
        return *this;
    }

    template<typename T>
    T& get() {
        ASSERT(&typeid(T) == m_holder->info);
        return *(T*)m_holder->getPtr();
    }
    template<typename T>
    const T& get() const {
        ASSERT(&typeid(T) == m_holder->info);
        return *(T*)m_holder->getPtr();
    }
    template<typename T>
    bool isTypeOf() const {
        return &typeid(T) == m_holder->info;
    }
private:
    IHolder *m_holder;
};

#endif
