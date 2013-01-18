#pragma once

#include <cassert>

#include <limits>
#include <vector>

// 这个默认实现是提供给int等不使用erase接口的类型
template<typename ValueT>
struct min_heap_traits
{
	typedef std::vector<ValueT>	VecT;

	typename VecT::size_type get_heap_pos(const ValueT& val){ return 0; }
	void set_heap_pos(ValueT& val, typename VecT::size_type pos){}
	bool compare(const ValueT& lhs, const ValueT& rhs){ return lhs < rhs; }
	const ValueT minimum() { return std::numeric_limits<ValueT>::min(); }
};

// 要使用erase, ValueT的行为要求近似于指针, 而且必须要实现min_heap_traits::set_heap_pos等
template<typename ValueT, typename Traits = min_heap_traits<ValueT> >
class min_heap
{
public:
	typedef typename Traits::VecT::allocator_type		allocator_type;
	typedef typename Traits::VecT::size_type			size_type;
	typedef typename Traits::VecT::value_type			value_type;

public:
    min_heap(const Traits& traits = Traits()): m_traits(traits) { m_vec.push_back(m_traits.minimum()); }
    min_heap(const allocator_type &_alloc, const Traits& traits = Traits()): m_traits(traits), m_vec(_alloc){ m_vec.push_back(m_traits.minimum()); }

	size_type size() const { return m_vec.size() - 1; }

	bool empty() const { return size() == 0; }

    void clear() { m_vec.resize(1); }

	void insert(value_type val)
	{
		m_vec.push_back(val);
		up(m_vec.size() - 1);
	}

	void erase(value_type val)
	{
		size_type pos = m_traits.get_heap_pos(val);
		assert(pos > 0 && pos < m_vec.size());
		assert(!m_traits.compare(val, m_vec[pos]) && !m_traits.compare(m_vec[pos], val));

		if(pos < m_vec.size() - 1)
		{
			std::swap(m_vec[pos], m_vec.back());

			if (pos == 1) down(pos, m_vec.size() - 1);
			else
			{
				if (m_traits.compare(m_vec[pos], m_vec[pos >> 1])) up(pos);
				else down(pos, m_vec.size() - 1);
			}
		}

		m_traits.set_heap_pos(m_vec.back(), 0);
		m_vec.pop_back();
	}

	const value_type top() const { assert(!empty()); return m_vec[1]; }
	value_type top() { assert(!empty()); return m_vec[1]; }

	void push(value_type val) { insert(val); }
	void pop() 
	{
		assert(!empty());

		if (size() > 1)
		{
			std::swap(m_vec[1], m_vec.back());
			down(1, m_vec.size() - 1);
		}

		m_traits.set_heap_pos(m_vec.back(), 0);
		m_vec.pop_back();
	}

private:
	void down(size_type pos, size_t maxSize)
	{
		assert(pos > 0 && pos < maxSize);

        size_type maxPos = (maxSize - 1) >> 1;
        value_type *begin = &m_vec[0];

        value_type val = begin[pos];

        while (pos <= maxPos)
        {
            size_type lChild = pos << 1;
            size_type rChild = lChild + 1;

            size_type minChild = lChild;
            if (rChild < maxSize && 
                m_traits.compare(begin[rChild], begin[lChild])) 
            {
                minChild = rChild;
            }

            if (!m_traits.compare(begin[minChild], val)) break;

            begin[pos] = begin[minChild];
            m_traits.set_heap_pos(begin[pos], pos);

            pos = minChild;
        }

        begin[pos] = val;
        m_traits.set_heap_pos(begin[pos], pos);
	}

	void up(size_type pos)
	{
		assert(pos > 0 && pos < m_vec.size());

        value_type *begin = &m_vec[0];
        value_type val = begin[pos];

        size_type parentPos = pos >> 1;
        while (m_traits.compare(val, begin[parentPos]))
        {
            begin[pos] = begin[parentPos];
            m_traits.set_heap_pos(begin[pos], pos);

            pos = parentPos;
            parentPos = pos >> 1;
        }

        begin[pos] = val;
        m_traits.set_heap_pos(begin[pos], pos);
	}

private:
	typename Traits::VecT		m_vec;
    Traits                      m_traits;
};