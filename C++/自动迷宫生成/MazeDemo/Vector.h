#pragma once

#include "Algorithm.h"
#include "TypeDef.h"

#include <cassert>
#include <exception>

template<typename Type>
class Vector
{
public:
    typedef const Type* Const_Iterator;
    typedef Type*       Iterator;

public:
    Vector();
    template<typename CommonIterator>
    Vector(CommonIterator itBegin, CommonIterator itEnd);
    Vector(uInt iSize, const Type& v = Type());
    Vector(const Vector& o);
    Vector& operator = (const Vector& o);
    ~Vector();
    
    template<typename CommonIterator>
    void assign(CommonIterator itBegin, CommonIterator itEnd);
    void resize(uInt iSize, const Type& v = Type());
    bool empty() const;
    uInt size() const;
    uInt capacity() const;
    Const_Iterator begin() const;
    Iterator begin();
    Const_Iterator end() const;
    Iterator end();
    const Type& back() const;
    Type& back();
    const Type& front() const;
    Type& front();
    const Type& operator [] (uInt i) const;
    Type& operator [] (uInt i);
    const Type& at(uInt i) const;
    Type& at(uInt i);
    void pushBack(const Type& v);
    void popBack();
    Iterator insert(Iterator it, const Type& v);
    template<typename CommonIterator>
    Iterator insert(Iterator it, CommonIterator itBegin, CommonIterator itEnd);
    Iterator erase(Iterator it);
    Iterator erase(Iterator itBegin, Iterator itEnd);
    void remove(const Type& v);
    void reserve(uInt iCapacity);
    void clear();
    void reverse();
    void reverse(Iterator itBegin, Iterator itEnd);

private:
    Type   *m_pData;
    uInt    m_iSize;
    uInt    m_iCapacity;
};

template<typename Type>
Vector<Type>::Vector():
    m_pData(NULL),
    m_iSize(0),
    m_iCapacity(0)
{
    reserve(1 << 2);
}

template<typename Type>
template<typename CommonIterator>
Vector<Type>::Vector(CommonIterator itBegin, CommonIterator itEnd):
    m_pData(NULL),
    m_iSize(0),
    m_iCapacity(0)
{
    reserve(distance(itBegin, itEnd));
    assign(itBegin, itEnd);
}

template<typename Type>
Vector<Type>::Vector(uInt iSize, const Type& v/* = Type()*/):
    m_pData(NULL),
    m_iSize(0),
    m_iCapacity(0)
{
    reserve(iSize);
    resize(iSize, v);
}

template<typename Type>
Vector<Type>::Vector(const Vector& o):
    m_pData(NULL),
    m_iSize(0),
    m_iCapacity(0)
{
    reserve(o.size());
    *this = o;
}   

template<typename Type>
Vector<Type>::~Vector()
{
    clear();
}

template<typename Type>
inline Vector<Type>& Vector<Type>::operator = (const Vector& o)
{
    if (this != &o)
    {
        assign(o.begin(), o.end());
    }
    return *this;
}

template<typename Type>
template<typename CommonIterator>
inline void Vector<Type>::assign(CommonIterator itBegin, CommonIterator itEnd)
{
    clear();
    insert(end(), itBegin, itEnd);
}

template<typename Type>
void Vector<Type>::resize(uInt iSize, const Type& v = Type())
{
    if (iSize >= capacity())
    {
        reserve(iSize);
    }
    m_iSize = iSize;
    for (uInt i = 0; i < m_iSize; ++i)
    {
        m_pData[i] = v;
    }
}

template<typename Type>
inline bool Vector<Type>::empty() const
{
    return size() == 0;
}

template<typename Type>
inline uInt Vector<Type>::size() const
{
    return m_iSize;
}

template<typename Type>
inline uInt Vector<Type>::capacity() const
{
    return m_iCapacity;
}

template<typename Type>
inline typename Vector<Type>::Const_Iterator Vector<Type>::begin() const
{
    return const_cast<Vector<Type>*>(this)->begin();
}

template<typename Type>
inline typename Vector<Type>::Iterator Vector<Type>::begin()
{
    return m_pData;
}

template<typename Type>
inline typename Vector<Type>::Const_Iterator Vector<Type>::end() const
{
    return const_cast<Vector<Type>*>(this)->end();
}

template<typename Type>
inline typename Vector<Type>::Iterator Vector<Type>::end()
{
    return m_pData + m_iSize;
}

template<typename Type>
inline const Type& Vector<Type>::back() const
{
    return const_cast<Vector<Type>*>(this)->back();
}

template<typename Type>
inline Type& Vector<Type>::back()
{
    return *(end() - 1);
}

template<typename Type>
inline const Type& Vector<Type>::front() const
{
    return const_cast<Vector<Type>*>(this)->front();
}

template<typename Type>
inline Type& Vector<Type>::front()
{
    return *begin();
}

template<typename Type>
inline void Vector<Type>::pushBack(const Type& v)
{
    if (size() == capacity())
    {
        reserve(2 * (size() + 1));
    }
    m_pData[m_iSize++] = v;
}

template<typename Type>
inline void Vector<Type>::popBack()
{
    assert(size() > 0);
    --m_iSize;
}

template<typename Type>
typename Vector<Type>::Iterator Vector<Type>::insert(Iterator it, const Type& v)
{
    assert(distance(begin(), it) <= size() && distance(begin(), it) >= 0);
    if (size() == capacity())
    {
        reserve(2 * size());
    }
    Iterator itEnd = ++end();
    while (--itEnd != it)
    {
        *itEnd = *(itEnd - 1);
    }
    *it = v;
    ++m_iSize;
    return it;
}

template<typename Type>
template<typename CommonIterator>
typename Vector<Type>::Iterator Vector<Type>::insert(Iterator it, CommonIterator itBegin, CommonIterator itEnd)
{
    assert(distance(begin(), it) <= size() && distance(begin(), it) >= 0);
    uInt iLen = distance(itBegin, itEnd);
    if (capacity() - size() <= iLen)
    {
        reserve(2 * size() + iLen);
    }
    Iterator itHead = end() - 1;
    Iterator itTail = itHead + iLen;
    while (itHead != it - 1)
    {
        *itTail-- = *itHead--;
    }
    while (itBegin != itEnd)
    {
        *++itHead = *itBegin++;
    }
    m_iSize += iLen;
    return it;
}

template<typename Type>
typename Vector<Type>::Iterator Vector<Type>::erase(Iterator it)
{
    assert(distance(begin(), it) <= size() && distance(begin(), it) >= 0);
    --m_iSize;
    Iterator itTemp = it - 1;
    while (++itTemp != end())
    {
        *itTemp = *(itTemp + 1);
    }
    return it;
}

template<typename Type>
typename Vector<Type>::Iterator Vector<Type>::erase(Iterator itBegin, Iterator itEnd)
{
    assert(distance(begin(), itBegin) >= 0 && distance(itEnd, end()) >= 0);
    int iLen = distance(itBegin, itEnd);
    Iterator itHead = itBegin;
    Iterator itTail = itEnd;
    while (itTail != end())
    {
        *itHead++ = *itTail++;
    }
    m_iSize -= iLen;
    return itBegin;
}

template<typename Type>
void Vector<Type>::remove(const Type& v)
{
    for (Iterator it = begin(); it != end(); ++it)
    {
        if (*it == v)
        {
            erase(it);
        }
    }
}

template<typename Type>
void Vector<Type>::reserve(uInt iCapacity)
{
    if (capacity() < iCapacity)
    {
        uInt iNewMemSize = sizeof(Type) * iCapacity;
        if (m_pData == NULL)
        {
            m_pData = static_cast<Type*>(malloc(iNewMemSize));
        }
        else
        {
            m_pData = static_cast<Type*>(realloc(m_pData, iNewMemSize));
        }
        for (uInt i = capacity(); i < iCapacity; ++i)
        {
            new (&m_pData[i]) Type();
        }
        m_iCapacity = iCapacity;
    }
}

template<typename Type>
void Vector<Type>::clear()
{
    for (uInt i = 0; i < m_iCapacity; ++i)
    {
        m_pData[i].~Type();
    }
    free(m_pData);
    m_pData     = NULL;
    m_iSize     = 0;
    m_iCapacity = 0;
}

template<typename Type>
inline const Type& Vector<Type>::operator [] (uInt i) const
{
    return (*const_cast<Vector<int>*>(this))[i];
}

template<typename Type>
inline Type& Vector<Type>::operator [] (uInt i)
{
    return m_pData[i];
}

template<typename Type>
inline const Type& Vector<Type>::at(uInt i) const
{
    return const_cast<Vector<Type>*>(this)->at(i);
}

template<typename Type>
inline Type& Vector<Type>::at(uInt i)
{
    if (i < 0 || i >= size())
    {
        throw std::out_of_range("Vector::at·µ»ØÒç³ö");
    }
    return m_pData[i];
}

template<typename Type>
inline void Vector<Type>::reverse()
{
    reverse(begin(), end());
}

template<typename Type>
void Vector<Type>::reverse(Iterator itBegin, Iterator itEnd)
{
    --itEnd;
    while (itBegin < itEnd)
    {
        xChange(*itBegin++, *--itEnd);
    }
}