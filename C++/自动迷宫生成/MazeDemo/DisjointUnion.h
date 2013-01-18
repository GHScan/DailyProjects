#pragma once

#include "Algorithm.h"
#include "DisjointUnion.h"

class DisjointUnion
{
public:
    DisjointUnion(int iSize, int iMin = 0);
    DisjointUnion(const DisjointUnion& o);
    ~DisjointUnion();
    DisjointUnion& operator = (const DisjointUnion& o);

    void reset();
    bool isJoint(int iFirst, int iSecond);
    void makeJoint(int iFirst, int iSecond);

    int  size() const;
    int  getMin() const;

private:
    void clear();
    int findRoot(int iPos);

private:
    int    *m_pTable;
    int     m_iMin;
    int     m_iSize;
};


inline DisjointUnion::DisjointUnion(int iSize, int iMin/* = 0*/):
    m_iSize(iSize),
    m_iMin(iMin),
    m_pTable(NULL)
{
    m_pTable = new int[m_iSize];
    reset();
}

inline DisjointUnion::DisjointUnion(const DisjointUnion& o):
    m_pTable(NULL),
    m_iSize(0),
    m_iMin(0)
{
    *this = o;
}

inline DisjointUnion::~DisjointUnion()
{
    clear();
}

inline DisjointUnion& DisjointUnion::operator = (const DisjointUnion& o)
{
    if (this != &o)
    {
        clear();

        m_iSize     = o.m_iSize;
        m_iMin      = o.m_iMin;
        if (o.m_pTable != NULL)
        {
            m_pTable = new int[m_iSize];
            memcpy(m_pTable, o.m_pTable, m_iSize * sizeof(int));
        }
    }
    return *this;
}

inline void DisjointUnion::reset()
{
    for (int i = 0; i < m_iSize; ++i)
    {
        m_pTable[i] = -1;
    }
}

inline void DisjointUnion::clear()
{
    delete m_pTable;
    m_pTable = NULL;
    m_iSize  = 0;
    m_iMin   = 0;
}

inline int DisjointUnion::findRoot(int iPos)
{
    if (m_pTable[iPos] < 0)
    {
        return iPos;
    }
    else
    {
        return m_pTable[iPos] = findRoot(m_pTable[iPos]);
    }
}

inline bool DisjointUnion::isJoint(int iFirst, int iSecond)
{
    iFirst  -= m_iMin;
    iSecond -= m_iMin;
    assert(iFirst < m_iSize && iFirst >= 0);
    assert(iSecond < m_iSize && iSecond >= 0);

    return findRoot(iFirst) == findRoot(iSecond);
}

inline void DisjointUnion::makeJoint(int iFirst, int iSecond)
{
    iFirst  -= m_iMin;
    iSecond -= m_iMin;
    assert(iFirst < m_iSize && iFirst >= 0);
    assert(iSecond < m_iSize && iSecond >= 0);

    int iMin = findRoot(iFirst);
    int iMax = findRoot(iSecond);
    if (m_pTable[iMin] < m_pTable[iMax])
    {
        xChange(iMin, iMax);
    }
    m_pTable[iMax]  += m_pTable[iMin];
    m_pTable[iMin]   = iMax;
}

inline int DisjointUnion::size() const
{
    return m_iSize;
}

inline int DisjointUnion::getMin() const
{
    return m_iMin;
}