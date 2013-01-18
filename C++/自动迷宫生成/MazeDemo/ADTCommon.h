#pragma once

struct RandomIteratorTag{};
struct SWayIteratorTag{};
struct DWayIteratorTag{};

template<typename Type>
struct IteratorTraits
{
    typedef typename Type::ValueType    ValueType;
    typedef typename Type::IteratorType IteratorType;
};
template<typename Type>
struct IteratorTraits<Type*>
{
    typedef Type                ValueType;
    typedef RandomIteratorTag   IteratorType;
};
template<typename Type>
struct IteratorTraits<const Type*>
{
    typedef Type                ValueType;
    typedef RandomIteratorTag   IteratorType;
};

template<typename FirstType, typename SecondType>
struct Pair
{
    FirstType   first;
    SecondType  second;
};