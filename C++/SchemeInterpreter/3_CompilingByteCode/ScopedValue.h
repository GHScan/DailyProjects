#ifndef SCOPEDVALUE_H
#define SCOPEDVALUE_H

class SValue;
class SObject;
class SExternalObject;

namespace ScopedValueHelp {
    template<typename T>
    struct ValidScopedType;

    template<>
    struct ValidScopedType<SValue> {
    };
    template<>
    struct ValidScopedType<SObject*> {
    };
    template<>
    struct ValidScopedType<SExternalObject*> {
    };
}

template<typename T>
class ScopedValue: public ScopedValueHelp::ValidScopedType<T> {
public:
    explicit ScopedValue(T v): value(v) {
        mNext = sFirst;
        sFirst = this;
    }

    ~ScopedValue() {
        ASSERT(sFirst == this);
        sFirst = mNext;
    }

    ScopedValue(const ScopedValue &o): value(o.value) {
        mNext = sFirst;
        sFirst = this;
    }

    ScopedValue& operator = (const ScopedValue &o) {
        value = o.value;
        return *this;
    }

    T value;

    static ScopedValue* getFirst() {
        return sFirst;
    } 

    ScopedValue* getNext() {
        return mNext;
    }

    static void* operator new (size_t) = delete;
    static void operator delete(void*) = delete;

private:
    ScopedValue *mNext;
    static ScopedValue *sFirst;
};

template<typename T>
ScopedValue<T>*  ScopedValue<T>::sFirst = nullptr;

#endif
