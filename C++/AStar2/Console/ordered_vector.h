#pragma once

#include <cassert>

#include <vector>
#include <algorithm>
#include <functional>

// 包装stl算法得到有序表
// 相比set/map, 只是实现不同而已, 参考:
// ordered_vector<false, int>                   -> set<int>
// ordered_vector<false, std::pair<int, int>>   -> map<int, int>
// ordered_vector<true, int>                    -> multiset<int>
// ordered_vector<true, std::pair<int, int>>    -> multimap<int, int>
template<bool multiValue, typename ValueT, typename LessT = std::less<ValueT>, typename VecT = std::vector<ValueT> >
class ordered_vector
{
public:
    typedef typename VecT::value_type               value_type;
    typedef typename LessT                          value_compare;
    typedef typename VecT::allocator_type           allocator_type;
    typedef typename VecT::size_type                size_type;
    typedef typename VecT::difference_type          difference_type;
    typedef typename VecT::pointer                  pointer;
    typedef typename VecT::const_pointer            const_pointer;
    typedef typename VecT::reference                reference;
    typedef typename VecT::const_reference          const_reference;
    typedef typename VecT::iterator                 iterator;
    typedef typename VecT::const_iterator           const_iterator;
    typedef typename VecT::reverse_iterator         reverse_iterator;
    typedef typename VecT::const_reverse_iterator   const_reverse_iterator;

private:
    template<bool b>
    struct MultiValueT{};

public:
    ordered_vector(){}
    explicit ordered_vector(const typename allocator_type& _alloc):
    m_vec(_alloc){}


    const_iterator begin() const { return m_vec.begin(); }
    iterator begin() { return m_vec.begin(); }

    const_iterator end() const { return m_vec.end(); }
    iterator end() { return m_vec.end(); }

    const_reverse_iterator rbegin() const { return m_vec.rbegin(); }
    reverse_iterator rbegin() { return m_vec.rbegin(); }

    const_reverse_iterator rend() const { return m_vec.rend(); }
    reverse_iterator rend() { return m_vec.rend(); }


    size_type max_size() const { return m_vec.max_size(); }

    size_type size() const { return m_vec.size(); }

    bool empty() const { return m_vec.empty(); }

    const_reference operator [] (size_type pos) const { return m_vec[pos]; }

    reference operator [] (size_type pos) { return m_vec[pos]; }

    void clear() { m_vec.clear(); }


    void swap(ordered_vector& o)
    {
        m_vec.swap(o.m_vec);
    }

    allocator_type get_allocator() const {  return m_vec.get_allocator(); }


    const_iterator lower_bound(const_reference val) const  
    { 
        return std::lower_bound(m_vec.begin(), m_vec.end(), val, m_cmp); 
    }
    iterator lower_bound(const_reference val)
    { 
        return std::lower_bound(m_vec.begin(), m_vec.end(), val, m_cmp); 
    }

    const_iterator upper_bound(const_reference val) const
    {
        return std::upper_bound(m_vec.begin(), m_vec.end(), val, m_cmp);
    }
    iterator upper_bound(const_reference val)
    {
        return std::upper_bound(m_vec.begin(), m_vec.end(), val, m_cmp);
    }

    std::pair<const_iterator, const_iterator> equal_range(const_reference val) const
    {
        return std::equal_range(m_vec.begin(), m_vec.end(), val, m_cmp);
    }
    std::pair<iterator, iterator> equal_range(const_reference val)
    {
        return std::equal_range(m_vec.begin(), m_vec.end(), val, m_cmp);
    }

    size_type count(const_reference val) const
    {
        std::pair<const_iterator, const_iterator> _pair = equal_range(val);
        return std::distance(_pair.first, _pair.second);
    }

    const_iterator find(const_reference val) const
    {
        const_iterator iter = lower_bound(val);
        if (iter != m_vec.end() && (m_cmp(*iter, val) || m_cmp(val, *iter))) return m_vec.end();
        return iter;
    }
    iterator find(const_reference val)
    {
        iterator iter = lower_bound(val);
        if (iter != m_vec.end() && (m_cmp(*iter, val) || m_cmp(val, *iter))) return m_vec.end();
        return iter;
    }


    iterator insert(const_reference val)
    {
        return insert(lower_bound(val), val);
    }

    iterator insert(iterator iterPos, const_reference val)
    {
        return insert_multi(iterPos, val, MultiValueT<multiValue>());
    }

    template<typename IterT>
    void insert(IterT _begin, IterT _end)
    {
        assert(std::distance(_begin, _end) >= 0);
        for (; _begin != _end; ++_begin)
        {
            insert(*_begin);
        }
    }

    iterator erase(const_reference val)
    {
        return m_vec.erase(find(val));
    }

    iterator erase(iterator iter)
    {
        return m_vec.erase(iter);
    }

    iterator erase(iterator _begin, iterator _end)
    {
        return m_vec.erase(_begin, _end);
    }

private:
    iterator insert_multi(iterator iterPos, const_reference val, MultiValueT<true>)
    {
        assert(std::distance(equal_range(val).first, iterPos) >= 0);
        assert(std::distance(iterPos, equal_range(val).second) >= 0);
        return m_vec.insert(iterPos, val);
    }

    iterator insert_multi(iterator iterPos, const_reference val, MultiValueT<false>)
    {
        assert(iterPos == lower_bound(val));
        if (iterPos == m_vec.end()) return m_vec.insert(iterPos, val);

        if (m_cmp(*iterPos, val) || m_cmp(val, *iterPos))
        {
            return m_vec.insert(iterPos, val);
        }
        else
        {
            *iterPos = val;
            return iterPos;
        }
    }

private:
    VecT            m_vec;
    value_compare   m_cmp;
};

// 用于辅助ordered_vector适配map: ordered_vector<false, std::pair<Key, Value>, key_value_less<std::pair<Key, Value>> >
template<typename PairT>
struct key_value_less
{
    bool operator () (const PairT& lhs, const PairT& rhs) const
    {
        return lhs.first < rhs.first;
    }
};