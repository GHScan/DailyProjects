#ifndef SCHEME_DYNAMIC_OBJECT_H
#define SCHEME_DYNAMIC_OBJECT_H

#include "TaggedPointer.h"
#include "BigInteger.h"

class SchemeMemoryManager;

class SchemeDynamicObject {
public:
    SchemeDynamicObject(const SchemeDynamicObject &o) = delete;
    SchemeDynamicObject& operator = (const SchemeDynamicObject &o) = delete;

    virtual ~SchemeDynamicObject() {
    }

    virtual bool equal(const SchemeDynamicObject& o) const = 0;

    bool isMarked() const {
        return next.get<TaggedPointer::TAG_MASK>() == 1;
    }

    void mark() {
        return next.set<TaggedPointer::TAG_MASK>(1);
    }

    void unmark() {
        return next.set<TaggedPointer::TAG_MASK>(0);
    }

    TaggedPointer next;

protected:
    SchemeDynamicObject() {
        next.set<~0>(0);
    }
};

class SchemeFloat: public SchemeDynamicObject {
public:
    double number;

    static SchemeFloat* create(SchemeMemoryManager *mgr, double n);
    static SchemeFloat* create(SchemeMemoryManager *mgr, const char *s);

    virtual bool equal(const SchemeDynamicObject& o) const override {
        if (auto p = dynamic_cast<const SchemeFloat*>(&o)) {
            return number == p->number;
        }
        return false;
    }

private:
    SchemeFloat(double n): number(n) {}
};

class SchemeString: public SchemeDynamicObject {
public:
    string str;

    static SchemeString* create(SchemeMemoryManager *mgr, const char *s);

    virtual bool equal(const SchemeDynamicObject& o) const override {
        if (auto p = dynamic_cast<const SchemeString*>(&o)) {
            return str == p->str;
        }
        return false;
    }

private:
    SchemeString(const char *s): str(s) {}
};

class SchemeBigInteger: public SchemeDynamicObject {
public:
    BigInteger number;

    static SchemeBigInteger* create(SchemeMemoryManager *mgr, const BigInteger &n);
    static SchemeBigInteger* create(SchemeMemoryManager *mgr, const char *s);

    virtual bool equal(const SchemeDynamicObject& o) const override {
        if (auto p = dynamic_cast<const SchemeBigInteger*>(&o)) {
            return number == p->number;
        }
        return false;
    }

private:
    SchemeBigInteger(const BigInteger &n): number(n) {}
};

#endif
