// vim:fileencoding=gbk

#include "pch.h"

#include <cassert>

#include <vector>

#include "SharedPtr.h"

int g_b = 0;
int g_b2 = 0;
int g_d = 0;

struct B
{
    B() { i = 1; ++g_b; }
    virtual ~B() { --g_b; }
    int i;
};

struct B2
{
    B2() { i = 2; ++g_b2; }
    virtual ~B2() { --g_b2; }
    int i;
};

struct D:
    public B, public B2
{
    D() { i = 3; ++g_d; }
    ~D() { --g_d; }
    int i;
};

void deleteBAndIncGB(B* p)
{
    delete p;
    ++g_b;
}

void testSharedPtr()
{
    assert(g_b == 0);
    {
        B *p = new B();
        assert(g_b == 1);
        SharedPtr<B> s(p);
        assert(s.refCount() == 1);
        SharedPtr<B> s2(s);
        assert(s.refCount() == 2);
        SharedPtr<B> s3;
        assert(s3 == NULL);
        s3 = s2;
        assert(s.refCount() == 3);
    }
    assert(g_b == 0);

    {
        SharedPtr<B>(new B(), &deleteBAndIncGB);
        assert(g_b == 1);
        --g_b;
    }

    assert(g_b == 0);
    {
        SharedPtr<B> s(new D);
        SharedPtr<B2> s2(s.dynamicCast<D>());
        assert(s2->i == 2);
    }

    assert(g_b2 == 0);
    {
        std::vector<SharedPtr<B2> > v(2);
        assert(g_b2 == 0);
        for (int i = 0; i < 2; ++i) v[i] = SharedPtr<D>(new D);
        assert(g_b2 == 2);
    }
    assert(g_b2 == 0);

    {
        SharedPtr<B2> a(new D);
        SharedPtr<B2> b(new B2);
        a->i = 1, b->i = 2;
        assert(a->i = 1);
        assert(b->i = 2);
        std::swap(a, b);
        assert(a->i = 2);
        assert(b->i = 1);
    }

    assert(g_b == 0);
    assert(g_b2 == 0);
    assert(g_d == 0);
}

void testWeakPtr()
{
    assert(g_b == 0);
    {
        WeakPtr<B> w;
        assert(w.weakRefCount() == 0);
        assert(!w.isValid());
        {
            SharedPtr<B> s(new D);
            w = s;
            assert(w.weakRefCount() == 2);
            assert(w.isValid());
            SharedPtr<B> s2(w.lock());
            assert(s.refCount() == 2);
            assert(w.weakRefCount() == 3);
            assert(g_b == 1);
        }
        assert(g_b == 0);
        assert(w.weakRefCount() == 1);
        assert(!w.isValid());
    }
    assert(g_b == 0);
}

int main()
{
    testSharedPtr();
    testWeakPtr();

    _CrtDumpMemoryLeaks();

    return 0;
}
