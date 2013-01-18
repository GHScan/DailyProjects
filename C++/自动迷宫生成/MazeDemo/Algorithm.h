#pragma once

#include "ADTCommon.h"
#include "TypeDef.h"

#include <cassert>

template<typename Iterator>
inline uInt distance(Iterator itBegin, Iterator itEnd)
{
    return distance(itBegin, itEnd, static_cast<IteratorTraits<Iterator>::IteratorType*>(0));
}

template<typename Iterator>
inline uInt distance(Iterator itBegin, Iterator itEnd, RandomIteratorTag*)
{
    return itEnd - itBegin;
}

template<typename Iterator>
inline uInt distance(Iterator itBegin, Iterator itEnd, DWayIteratorTag*)
{
    return distance(itBegin, itEnd, static_cast<SWayIteratorTag*>(0));
}

template<typename Iterator>
inline uInt distance(Iterator itBegin, Iterator itEnd, SWayIteratorTag*)
{
    uInt iRet = 0;
    while (itBegin != itEnd)
    {
        ++itBegin;
        ++iRet;
    }
    return iRet;
}

template<typename Iterator>
inline Iterator& advance(Iterator& it, int iValue)
{
    return advance(it, iValue, static_cast<IteratorTraits<Iterator>::IteratorType*>(0));
}

template<typename Iterator>
inline Iterator& advance(Iterator& it, int iValue, RandomIteratorTag*)
{
    return it += iValue;
}

template<typename Iterator>
Iterator& advance(Iterator& it, int iValue, DWayIteratorTag*)
{
    if (iValue > 0)
    {
        while (iValue != 0)
        {
            ++it;
            --iValue;
        }
    }
    else
    {
        while (iValue != 0)
        {
            --it;
            ++iValue;
        }
    }
    return it;
}

template<typename Iterator>
Iterator& advance(Iterator& it, uInt iValue, SWayIteratorTag*)
{
    assert(iValue > 0);
    while (iValue != 0)
    {
        ++it;
        --iValue;
    }
    return it;
}

template<typename Type>
inline void xChange(Type& lhs, Type& rhs)
{
    Type t  = lhs;
    lhs     = rhs;
    rhs     = t;
}

template<typename Type>
inline const Type& maximum(const Type& lhs, const Type& rhs)
{
    return lhs < rhs ? rhs : lhs;
}

template<typename Type>
inline const Type& minimum(const Type& lhs, const Type& rhs)
{
    return lhs < rhs ? lhs : rhs;
}

struct FLower
{
    template<typename Type>
    bool operator () (const Type& lhs, const Type& rhs)
    {
        return lhs < rhs;
    }
};

struct FGreater
{
    template<typename Type>
    bool operator () (const Type& lhs, const Type& rhs)
    {
        return lhs > rhs;
    }
};

template<typename Iterator>
inline void quickSortSlow(Iterator itBegin , Iterator itEnd)
{
    quickSortSlow(itBegin, itEnd, FLower());
}

template<typename Iterator, typename FCompare>
void quickSortSlow(Iterator itBegin , Iterator itEnd, FCompare f)
{
    Iterator itLo = itBegin, itHi = itEnd;
    if (itLo == itHi || itLo == --itHi)
    {
        return;
    }

    IteratorTraits<Iterator>::ValueType v = *itLo;
    while (true)
    {
        while (!f(*itHi, v) && itLo != itHi)
        {
            --itHi;
        }
        if (itLo == itHi)
        {
            break;
        }
        *itLo++ = *itHi;
        while (f(*itLo, v) && itLo != itHi)
        {
            ++itLo;
        }
        if (itLo == itHi)
        {
            break;
        }
        *itHi-- = *itLo;
    }
    *itLo = v;
    quickSortSlow(itBegin, itLo);
    quickSortSlow(++itLo, itEnd);
}

template<typename CommonIterator>
inline void bubbleSort(CommonIterator itBegin, CommonIterator itEnd)
{
    bubbleSort(itBegin, itEnd, FLower());
}

template<typename CommonIterator, typename FCompare>
void bubbleSort(CommonIterator itBegin, CommonIterator itEnd, FCompare f)
{
    assert(itBegin != itEnd);

    for (CommonIterator itStop = itEnd - 1; itStop != itBegin; --itStop)
    {
        bool bSorted = true;
        for (CommonIterator it = itBegin; it != itStop; ++it)
        {
            if (!f(*it, *(it + 1)))
            {
                xChange(*it, *(it + 1));
                bSorted = false;
            }
        }
        if (bSorted)
        {
            break;
        }
    }
}

template<typename CommonIterator>
inline void insertionSort(CommonIterator itBegin, CommonIterator itEnd)
{
    insertionSort(itBegin, itEnd, FLower());
}   

template<typename CommonIterator, typename FCompare>
void insertionSort(CommonIterator itBegin, CommonIterator itEnd, FCompare f)
{
    assert(itBegin != itEnd);

    for (CommonIterator itStop = itBegin + 1; itStop != itEnd; ++itStop)
    {
        IteratorTraits<CommonIterator>::ValueType v = *itStop;
        CommonIterator it = itStop + 1;
        while (--it != itBegin)
        {
            if (!f(*(it - 1), v))
            {
                *it = *(it - 1);
            }
            else
            {
                break;
            }
        }
        *it = v;
    }
}

template<typename CommonIterator>
inline void selectionSort(CommonIterator itBegin, CommonIterator itEnd)
{
    selectionSort(itBegin, itEnd, FLower());
}

template<typename CommonIterator, typename FCompare>
void selectionSort(CommonIterator itBegin, CommonIterator itEnd, FCompare f)
{
    assert(itBegin != itEnd);

    for (CommonIterator itMinPos = itBegin; itMinPos != itEnd - 1; ++itMinPos)
    {
        CommonIterator itMin = itMinPos;
        for (CommonIterator it = itMin + 1; it != itEnd; ++it)
        {
            if (f(*it, *itMin))
            {
                itMin = it;
            }
        }
        if (itMin != itMinPos)
        {
            xChange(*itMin, *itMinPos);
        }
    }
}

template<typename CommonIterator>
inline void shellSort(CommonIterator itBegin, CommonIterator itEnd)
{
    shellSort(itBegin, itEnd, FLower());
}

template<typename CommonIterator, typename FCompare>
void shellSort(CommonIterator itBegin, CommonIterator itEnd, FCompare f)
{
    assert(itBegin != itEnd);

    static const uInt s_vStep[] = 
    {
        (1u << 1)  - 1, (1u << 2 ) - 1, (1u << 3 ) - 1, (1u << 4 ) - 1,
        (1u << 5 ) - 1, (1u << 6 ) - 1, (1u << 7 ) - 1, (1u << 8 ) - 1,
        (1u << 9 ) - 1, (1u << 10) - 1, (1u << 11) - 1, (1u << 12) - 1,
        (1u << 13) - 1, (1u << 14) - 1, (1u << 15) - 1, (1u << 16) - 1,
        (1u << 17) - 1, (1u << 18) - 1, (1u << 19) - 1, (1u << 20) - 1,
        (1u << 21) - 1, (1u << 22) - 1, (1u << 23) - 1, (1u << 24) - 1,
        (1u << 25) - 1, (1u << 26) - 1, (1u << 27) - 1, (1u << 28) - 1,
        (1u << 29) - 1, (1u << 30) - 1, (1u << 31) - 1, ~0,
    };

    uInt iCount     = uInt(itEnd - itBegin);
    int  iStepIndex = 31;
    while (iStepIndex > 0 && iCount <= s_vStep[iStepIndex])
    {
        --iStepIndex;
    }
    while (iStepIndex >= 0)
    {
        uInt iStep = s_vStep[iStepIndex];
        CommonIterator itNew = itBegin + iStep;
        while (itNew != itEnd)
        {
            IteratorTraits<CommonIterator>::ValueType v = *itNew;
            CommonIterator it = itNew - iStep;
            while (it >= itBegin)
            {
                if (!f(*it, v))
                {
                    *(it + iStep) = *it;
                    it -= iStep;
                }
                else
                {
                    break;
                }
            }
            *(it + iStep) = v;
            ++itNew;
        }
        --iStepIndex;
    }
}

template<typename CommonIterator>
inline void mergeSort(CommonIterator itBegin , CommonIterator itEnd)
{
    mergeSort(itBegin, itEnd, FLower());
}

template<typename CommonIterator, typename FCompare>
inline void mergeSort(CommonIterator itBegin , CommonIterator itEnd, FCompare f)
{
    typedef IteratorTraits<CommonIterator>::ValueType ValueType;

    assert(itBegin != itEnd);

    uInt iCount         = uInt(itEnd - itBegin);
    ValueType *pTemp    = new ValueType[iCount];
    mergeSortKernel(itBegin, iCount, f, pTemp);
    delete[] pTemp;
}

template<typename CommonIterator, typename FCompare>
void mergeSortKernel(CommonIterator itBegin , 
                     uInt iCount, 
                     FCompare f, 
                     typename IteratorTraits<CommonIterator>::ValueType *pTemp)
{
    if (iCount <= 1)
    {
        return;
    }

    uInt iMid                   = iCount >> 1;
    CommonIterator itLeft       = itBegin;
    CommonIterator itLeftEnd    = itBegin + iMid;
    CommonIterator itRight      = itLeftEnd;
    CommonIterator itRightEnd   = itRight + iCount - iMid;

    mergeSortKernel(itLeft  , uInt(itLeftEnd  - itLeft)   , f, pTemp);
    mergeSortKernel(itRight , uInt(itRightEnd - itRight)  , f, pTemp);
    
    IteratorTraits<CommonIterator>::ValueType *pCur = pTemp;
    while (itLeft != itLeftEnd && itRight != itRightEnd)
    {
        CommonIterator &it = f(*itLeft, *itRight) ? itLeft : itRight; 
        *pCur++ = *it++;
    }
    while (itLeft != itLeftEnd)
    {
        *pCur++ = *itLeft++;
    }
    while (itRight != itRightEnd)
    {
        *pCur++ = *itRight++;
    }
    while (iCount-- != 0)
    {
        *itBegin++ = *pTemp++;
    }
};

template<typename CommonIterator>
inline void bucketSort(CommonIterator itBegin, CommonIterator itEnd)
{
    bucketSort(reinterpret_cast<uLong*>(itBegin), reinterpret_cast<uLong*>(itEnd), FLower());
}

template<typename FCompare>
void bucketSort(uLong *itBegin, uLong *itEnd, FCompare f, uLong iMin = 0, uLong iMax = ~0)
{
    iMax = iMax == ~0 ? uLong(itEnd - itBegin) : iMax;
    uLong iLen      = uLong(iMax - iMin);
    uLong *pBucket  = new uLong[iLen];

    for (uInt i = 0; i < iLen; ++i)
    {
        pBucket[i] = 0;
    }
    for (uLong *itCur = itBegin; itCur != itEnd; ++itCur)
    {
        ++pBucket[*itCur - iMin];
    }
    for (uLong i = 0; i < iLen; ++i)
    {
        while (pBucket[i]-- != 0)
        {
            *itBegin++ = i + iMin;
        }
    }

    delete[] pBucket;
};

template<typename RandomIterator>
inline void quickSort(RandomIterator itBegin, RandomIterator itEnd)
{
    quickSort(itBegin, itEnd, FLower());
}

template<typename RandomIterator, typename FCompare>
inline void quickSort(RandomIterator itBegin, RandomIterator itEnd, FCompare f)
{
    assert(itBegin != itEnd);

    quickSortKernel(itBegin, itEnd, f);
};

template<typename RandomIterator, typename FCompare>
void quickSortKernel(RandomIterator itBegin, RandomIterator itEnd, FCompare f)
{
    RandomIterator itLast   = itEnd - 1;
    uInt iCount             = uInt(itEnd - itBegin);

    if (iCount < 3)
    {
        if (iCount == 2 && !f(*itBegin, *itLast)) 
        {
            xChange(*itBegin, *itLast);
        }
        return;
    }
    
    RandomIterator itMiddle = itBegin + (iCount >> 1);
    if (!f(*itBegin, *itMiddle))
    {
        xChange(*itBegin, *itMiddle);
    }
    if (!f(*itBegin, *itLast))
    {
        xChange(*itBegin, *itLast);
    }
    if (!f(*itLast, *itMiddle))
    {
        xChange(*itMiddle, *itLast);
    }

    RandomIterator itLeft   = itBegin;
    RandomIterator itRight  = itLast;
    while (true)
    {
        while (f(*++itLeft  , *itLast));
        while (f(*itLast     , *--itRight));
        if (itLeft < itRight)
        {
            xChange(*itLeft, *itRight);
        }
        else
        {
            break;
        }
    }
    xChange(*itLeft, *itLast);

    quickSortKernel(itBegin, itLeft, f);
    quickSortKernel(itLeft + 1, itEnd, f);
}

// k = 0...n
template<typename RandomIterator, typename RetType>
inline void findKth(RandomIterator itBegin, RandomIterator itEnd, uLong k, RetType& v)
{
    findKth(itBegin, itEnd, k, v, FLower());
};

template<typename RandomIterator, typename RetType, typename FCompare>
inline void findKth(RandomIterator itBegin, RandomIterator itEnd, uLong k, RetType& v, FCompare f)
{
    typedef IteratorTraits<RandomIterator>::ValueType ValueType;
    uLong iCount = uLong(itEnd - itBegin);
    assert(iCount > k);

    ValueType *pTemp = new ValueType[iCount];
    ValueType *pCur  = pTemp;
    while (itBegin != itEnd)
    {
        *pCur++ = *itBegin++;
    }
    findKthKernel(pTemp, iCount, k, f);
    v = RetType(pTemp[k]);
    delete[] pTemp;
};

template<typename ValueType, typename FCompare>
void findKthKernel(ValueType *itBegin, uLong iCount, uLong k, FCompare f)
{
    if (k >= iCount)
    {
        return;
    }

    ValueType *itLast = itBegin + iCount - 1;
    if (iCount < 3)
    {
        if (iCount == 2 && !f(*itBegin, *itLast))
        {
            xChange(*itBegin, *itLast);
        }
        return;
    }

    ValueType *itMiddle = itBegin + (iCount >> 1);
    if (!f(*itBegin, *itMiddle))
    {
        xChange(*itBegin, *itMiddle);
    }
    if (!f(*itBegin, *itLast))
    {
        xChange(*itBegin, *itLast);
    }
    if (!f(*itLast, *itMiddle))
    {
        xChange(*itMiddle, *itLast);
    }

    ValueType *itLeft   = itBegin;
    ValueType *itRight  = itLast;
    while (true)
    {
        while (f(*++itLeft, *itLast));
        while (f(*itLast, *--itRight));
        if (itLeft < itRight)
        {
            xChange(*itLeft, *itRight);
        }
        else
        {
            break;
        }
    }
    xChange(*itLeft, *itLast);

    uLong iFindPos = uLong(itLeft - itBegin);
    if (iFindPos > k)
    {
        findKthKernel(itBegin, iFindPos, k, f);
    }
    else 
    if (iFindPos < k)
    {
        findKthKernel(itLeft + 1, iCount - iFindPos - 1, k - iFindPos - 1, f);
    }
}

template<typename RandomIterator>
inline void quickSortAsm(RandomIterator itBegin , RandomIterator itEnd)
{
    quickSortAsmKernel(reinterpret_cast<uLong*>(itBegin), reinterpret_cast<uLong*>(itEnd));
};

void quickSortAsmKernel(uLong *itBegin, uLong *itEnd)
{
    __asm
    {
        mov ecx, itEnd;
        sub ecx, itBegin;
        cmp ecx, 8;
        ja  _start;
        cmp ecx, 8;
        jne _end;
        mov ecx, itBegin;
        mov eax, [ecx];
        mov ebx, [ecx + TYPE uLong];
        cmp eax, ebx;
        jb  _end;
        mov [ecx], ebx;
        mov [ecx + TYPE uLong], eax;
        jmp _end;
_start:
        shr ecx, 1;
        and ecx, 0xfffffffc;
        mov edx, itBegin;
        mov eax, [edx];
        add edx, ecx;
        mov ebx, [edx];
        cmp eax, ebx;
        jbe _next1;
        mov [edx], eax;
        sub edx, ecx;
        mov [edx], ebx;
_next1:
        mov edx, itBegin;
        mov eax, [edx];
        mov edx, itEnd;
        mov ebx, [edx - TYPE uLong];
        cmp eax, ebx;
        jbe _next2;
        mov [edx - TYPE uLong], eax;
        mov edx, itBegin;
        mov [edx], ebx;
_next2:
        mov edx, itEnd;
        mov eax, [edx - TYPE uLong];
        mov edx, itBegin;
        add edx, ecx;
        mov ebx, [edx];
        cmp eax, ebx;
        jbe _next3;
        mov [edx], eax;
        mov edx, itEnd;
        mov [edx - TYPE uLong], ebx;
_next3:
        mov ecx, itBegin;
        mov edx, itEnd;
        sub edx, TYPE uLong;
_inLoop:
        mov ebx, itEnd;
        mov ebx, [ebx - TYPE uLong];
_lowLoop:
        add ecx, TYPE uLong;
        mov eax, [ecx];
        cmp eax, ebx;
        jb _lowLoop;
_highLoop:
        sub edx, TYPE uLong;
        mov eax, [edx];
        cmp eax, ebx;
        ja  _highLoop;
        cmp ecx, edx;
        ja _outLoop;
        mov eax, [ecx];
        mov ebx, [edx];
        mov [ecx], ebx;
        mov [edx], eax;
        jmp _inLoop;
_outLoop:
        mov edx, itEnd;
        sub edx, TYPE uLong;
        mov eax, [ecx];
        mov ebx, [edx];
        mov [ecx], ebx;
        mov [edx], eax;
        push ecx;
        push itBegin;
        call quickSortAsmKernel;
        pop ecx;
        pop ecx;
        push itEnd;
        add ecx, TYPE uLong;
        push ecx;
        call quickSortAsmKernel;
        pop ecx;
        pop ecx;
_end:
    }
}

template<typename RandomIterator>
inline void largeObjectSort(RandomIterator itBegin, RandomIterator itEnd)
{
    largeObjectSort(itBegin, itEnd, FLower());
}

template<typename RandomIterator, typename FCompare>
void largeObjectSort(RandomIterator itBegin, RandomIterator itEnd, FCompare f)
{
    typedef IteratorTraits<RandomIterator>::ValueType   ValueType;

    uInt iCount = uInt(itEnd - itBegin);
    assert(iCount > 0);

    RandomIterator *pTemp = new RandomIterator[iCount];
    for (uInt i = 0; i < iCount; ++i)
    {
        pTemp[i] = itBegin + i;
    }

    class IteratorCompare
    {
    public:
        explicit IteratorCompare(FCompare f):
            m_f(f)
        {

        }
        bool operator () (const RandomIterator& lhs, const RandomIterator& rhs)
        {
            return m_f(*lhs, *rhs);
        }
    private:
        FCompare    m_f;
    };

    quickSort(pTemp, pTemp + iCount, IteratorCompare(f));

    for (uInt i = 0; i < iCount; ++i)
    {
        if (pTemp[i] != itBegin + i)
        {
            ValueType v = *(itBegin + i);
            uInt itCur = i;
            while (pTemp[itCur] != itBegin + i)
            {
                *(itBegin + itCur)  = *pTemp[itCur];
                uInt itNext         = uInt(pTemp[itCur] - itBegin);
                pTemp[itCur]        = itBegin + itCur;
                itCur               = itNext;
            }
            pTemp[itCur]    = itBegin + itCur;
            *pTemp[itCur]   = v;
        }
    }

    delete[] pTemp;
}