#ifndef LINQ_H
#define LINQ_H

#include <cassert>

#include <utility>
#include <functional>
#include <memory>
#include <algorithm>

#include <set>
#include <vector>
#include <map>

/*
 * ISSUES:
 *  1. an non-delay action will break of the delay list: dataSource changed,
 *  but query doesn't know. (see @ non-delay)
 */

template<typename T>
struct DeclareType
{
    typedef 
        typename std::remove_cv<
        typename std::remove_reference<
        typename std::remove_cv<T>::type>::type>::type Type;
};

template<typename T> class Enumerable;
template<typename T>
auto range(T end) -> Enumerable<T>;
template<typename ContainerT>
auto from(const ContainerT& c) -> Enumerable<
    typename DeclareType<decltype(*std::begin(c))>::Type >;

template<typename T>
class Enumerable
{
private:
    typedef std::function<std::pair<bool, T>()> ClosureT;
public:
    struct iterator
    {
    public:
        typedef std::forward_iterator_tag iterator_category;
        typedef T value_type;
        typedef int difference_type;
        typedef T* pointer;
        typedef T& reference;
    public:
        iterator(): m_advanced(false){}
        iterator(const ClosureT& c): m_closure(c), m_advanced(true)
        { 
            assert(m_closure);
        }
        iterator& operator ++ ()
        {
            _doAdvance();
            assert(m_closure && !m_advanced);
            m_advanced = true;
            return *this;
        }
        iterator operator ++ (int)
        {
            iterator r(*this);
            ++*this;
            return r;
        }
        const T& operator * () const
        {
            _doAdvance();
            return m_v;
        }
        bool operator == (const iterator& o) const
        {
            _doAdvance();
            o._doAdvance();
            // just for exp: begin == end
            return m_closure == nullptr && o.m_closure == nullptr;
        }
        bool operator != (const iterator& o) const
        {
            return !(*this == o);
        }

    private:
        void _doAdvance() const
        {
            const_cast<iterator*>(this)->_doAdvance();
        }
        void _doAdvance()
        {
            if (!m_advanced) return;
            m_advanced = false;
            assert(m_closure);
            auto r = m_closure();
            if (!r.first) m_closure = nullptr;
            else m_v = r.second;
        }

        ClosureT m_closure;
        T m_v;
        bool m_advanced;
    };

public:
    Enumerable(
            const ClosureT& c):
        m_closure(c)
    { }

    Enumerable() = default;

public:
    iterator begin() const
    {
        return iterator(m_closure);
    }
    iterator end() const
    {
        return iterator();
    }

public:
    template<typename FuncT>
    auto select(const FuncT &f) const -> Enumerable<typename DeclareType<decltype(f(*(T*)0))>::Type> 
    {
        typedef typename DeclareType<decltype(f(*(T*)0))>::Type RType;
        auto iter = this->begin(), end = this->end();
        return Enumerable<RType>([iter, end, f]() mutable
        {
            if (iter == end) return std::make_pair(false, RType());
            return std::make_pair(true, f(*iter++));
        });
    }

    template<typename FuncT>
    auto where(const FuncT& f) const -> Enumerable
    {
        auto iter = this->begin(), end = this->end();
        return Enumerable([iter, end, f]() mutable
        {
            while (iter != end && !f(*iter)) ++iter;
            if (iter == end) return std::make_pair(false, T());
            return std::make_pair(true, *iter++);
        });
    }

    template<typename FuncT>
    auto all(const FuncT& f) const -> bool
    {
        for (auto i : *this) {
            if (!f(i)) return false;
        }
        return true;
    }

    template<typename FuncT>
    auto any(const FuncT& f) const -> bool
    {
        for (auto i : *this) {
            if (f(i)) return true;
        }
        return false;
    }

    template<typename DestT>
    auto cast() const -> Enumerable<DestT>
    {
        auto iter = this->begin(), end = this->end();
        return Enumerable<DestT>([iter, end]() mutable
        {
            if (iter == end) return std::make_pair(false, DestT());
            return std::make_pair(true, DestT(*iter++));
        });
    }

    auto average() const -> T
    {
        T v = T();
        int n = 0;
        for (auto i : *this) {
            v += i;
            ++n;
        }
        assert(n > 0);
        return v / n;
    }

    auto contain(const T& v) const -> bool
    {
        for (auto i : *this) {
            if (i == v) return true;
        }
        return false;
    }

    auto count(const T& v) const -> int
    {
        return count([v](T i){ return i == v;});
    }

    template<typename FuncT>
    auto count(const FuncT& f, typename std::enable_if<!std::is_convertible<FuncT, T>::value>::type* = 0) const -> int
    {
        int n = 0;
        for (auto i : *this) {
            if (f(i)) ++n;
        }
        return n;
    }

    auto first() const -> T
    {
        return *this->begin();
    }

    auto last() const -> T
    {
        T v;
        for (auto i : *this) v = i;
        return v;
    }

    auto head(int n) const -> Enumerable
    {
        auto iter = this->begin(), end = this->end();
        return Enumerable([iter, end, n]() mutable
        {
            if (--n < 0 || iter == end) return std::make_pair(false, T());
            return std::make_pair(true, *iter++);
        });
    }

    auto tail(int n) const -> Enumerable
    {
        int sz = (int)std::vector<T>(this->begin(), this->end()).size();
        n = std::min(n, sz);
        auto iter = this->begin(), end = this->end();
        std::advance(iter, sz - n);
        return Enumerable([iter, end]() mutable
        {
            if (iter == end) return std::make_pair(false, T());
            return std::make_pair(true, *iter++);
        });
    }

    // @ non-delay
    template<typename FuncT>
    auto groupBy(const FuncT &f) const -> Enumerable<
        std::pair<typename DeclareType<decltype(f(*(T*)0))>::Type, Enumerable>> 
    {
        typedef typename DeclareType<decltype(f(*(T*)0))>::Type RType;
        typedef std::pair<RType, Enumerable> RPair;

        std::map<RType, std::vector<T>> m;
        for (auto i : *this) {
            m[f(i)].push_back(i);
        }

        std::shared_ptr<std::map<RType, Enumerable>> m2(new std::map<RType, Enumerable>());
        for (auto i : m) {
            (*m2)[i.first] = from(i.second).reserve();
        }

        auto iter = m2->begin();
        return Enumerable<RPair>([iter, m2]() mutable
        {
            if (iter == m2->end()) return std::make_pair(false, RPair());
            return std::make_pair(true, RPair(*iter++));
        });
    }

    template<typename FuncT>
    auto takeUntil(const FuncT& f) const -> Enumerable
    {
        auto iter = this->begin(), end = this->end();
        return Enumerable([iter, end, f]() mutable
        {
            if (iter == end) return std::make_pair(false, T());
            T r = *iter++;
            if (f(r)) return std::make_pair(false, T());
            return std::make_pair(true, r);
        });
    }

    template<typename FuncT>
    auto skipUntil(const FuncT& f) const -> Enumerable
    {
        auto iter = this->begin(), end = this->end();
        while (iter != end && !f(*iter)) ++iter;
        return Enumerable([iter, end]() mutable
        {
            if (iter == end) return std::make_pair(false, T());
            return std::make_pair(true, *iter++);
        });
    }

    auto max() const -> T
    {
        auto iter = this->begin(), end = this->end();
        assert(iter != end);
        T v = *iter++;
        while (iter != end) v = std::max(v, *iter++);
        return v;
    }

    auto min() const -> T
    {
        auto iter = this->begin(), end = this->end();
        assert(iter != end);
        T v = *iter++;
        while (iter != end) v = std::min(v, *iter++);
        return v;
    }

    template<typename FuncT>
    auto reduce(const FuncT& f, T v = T()) const -> T
    {
        for (auto i : *this) v = f(v, i);
        return v;
    }

    // @ non-delay
    auto reverse() const -> Enumerable
    {
        std::shared_ptr<std::vector<T>> v(new std::vector<T>(this->begin(), this->end()));
        auto iter = v->rbegin();
        return Enumerable([iter, v]() mutable
        {
            if (iter == v->rend()) return std::make_pair(false, T());
            return std::make_pair(true, *iter++);
        });
    }

    // @ non-delay
    auto reserve() const -> Enumerable
    {
        std::shared_ptr<std::vector<T>> v(new std::vector<T>(this->begin(), this->end()));
        auto iter = v->begin();
        return Enumerable([iter, v]() mutable
        {
            if (iter == v->end()) return std::make_pair(false, T());
            return std::make_pair(true, *iter++);
        });
    }

    auto sort() const -> Enumerable
    {
        return sort(std::less<T>());
    }

    // @ non-delay
    template<typename FuncT>
    auto sort(const FuncT& f) const -> Enumerable
    {
        std::shared_ptr<std::vector<T>> v(new std::vector<T>(this->begin(), this->end()));
        std::sort(v->begin(), v->end(), f);
        auto iter = v->begin();
        return Enumerable([iter, v]() mutable
        {
            if (iter == v->end()) return std::make_pair(false, T());
            return std::make_pair(true, *iter++);
        });
    }
    
    // @ non-delay
    auto intersect(const Enumerable& o) const -> Enumerable
    {
        std::shared_ptr<std::set<T>> s1(new std::set<T>(this->begin(), this->end()));
        std::shared_ptr<std::set<T>> s2(new std::set<T>(o.begin(), o.end()));
        auto iter = s1->begin();
        return Enumerable([iter, s1, s2]() mutable
        {
            while (iter != s1->end() && s2->count(*iter) == 0) ++iter;
            if (iter == s1->end()) return std::make_pair(false, T());
            return std::make_pair(true, *iter++);
        });
    }

    // @ non-delay
    auto _union(const Enumerable& o) const -> Enumerable
    {
        std::shared_ptr<std::set<T>> s(new std::set<T>(this->begin(), this->end()));
        s->insert(o.begin(), o.end());
        auto iter = s->begin();
        return Enumerable([iter, s]() mutable
        {
            if (iter == s->end()) return std::make_pair(false, T());
            return std::make_pair(true, *iter++);
        });
    }

    auto unique() const -> Enumerable 
    {
        std::set<T> s;
        auto iter = this->begin(), end = this->end();
        return Enumerable([iter, end, s]() mutable
        {
            while (iter != end && s.count(*iter) > 0) ++iter;
            if (iter == end) return std::make_pair(false, T());
            s.insert(*iter);
            return std::make_pair(true, *iter++);
        });
    }

    // @ non-delay
    auto random() const -> Enumerable
    {
        std::shared_ptr<std::vector<T>> v(new std::vector<T>(this->begin(), this->end()));
        std::random_shuffle(v->begin(), v->end());
        auto iter = v->begin();
        return Enumerable([iter, v]() mutable
        {
            if (iter == v->end()) return std::make_pair(false, T());
            return std::make_pair(true, *iter++);
        });
    }

private:
    ClosureT m_closure;
};

template<typename ContainerT>
auto from(const ContainerT& c) -> Enumerable<
    typename DeclareType<decltype(*std::begin(c))>::Type >
{
    typedef typename DeclareType<decltype(*std::begin(c))>::Type RType;
    bool init = false;
    auto iter = std::end(c);
    return Enumerable<RType>([init, iter, &c]() mutable
    {
        if (!init) { 
            init = true;
            iter = std::begin(c);
        }
        if (iter == std::end(c)) return std::make_pair(false, RType());
        return std::make_pair(true, *iter++);
    });
}

template<typename T>
auto range(T begin, T end, T step = 1) -> Enumerable<T>
{
    T cur = begin;
    return Enumerable<T>([cur, end, step]() mutable
    {
        if ((step > 0 && cur >= end) || (step < 0 && cur <= end)) {
            return std::make_pair(false, T());
        }
        T r = cur;
        cur += step;
        return std::make_pair(true, r);
    });
}

template<typename T>
auto range(T end) -> Enumerable<T>
{
    return range(T(), end);
}

#endif
