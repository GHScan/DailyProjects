#pragma once

#include "Types.h"

namespace Scan
{
    namespace MPL
    {
        template<typename DestType, typename SrcType>
        void assertTypeCast()
        {
            DestType *p = (SrcType*)0;
        }
#define SCAN_ASSERT_TYPECAST(dest, src)  (void(*)())Scan::MPL::assertTypeCast<dest, src>

        /**
            @brief 去掉类型的const修饰符
        */
        template<typename T>
        struct ToNoConst
        {
            typedef T Type;
        };
        template<typename T>
        struct ToNoConst<const T>
        {
            typedef T Type;
        };

        /**
            @brief 容器的特征
        */
        template<typename ContainerType, bool reverse>      
        struct ContainerTraits      
        {      
            typedef typename ContainerType::value_type   ValueType;      
            typedef typename ContainerType::iterator     IterType;

            static IterType begin(ContainerType& c)
            {
                return c.begin();
            }
            static IterType end(ContainerType& c)
            {
                return c.end();
            }
            static IterType& move(IterType& i, int step = 1)
            {
                std::advance(i, step);
                return i;
            }
            static uint32 getSize(const ContainerType& conn)
            {
                return (uint32)conn.size();
            }
        };  
        template<typename ContainerType>      
        struct ContainerTraits<ContainerType, true>
        {      
            typedef typename ContainerType::value_type          ValueType;      
            typedef typename ContainerType::reverse_iterator    IterType;

            static IterType begin(ContainerType& c)
            {
                return c.rbegin();
            }
            static IterType end(ContainerType& c)
            {
                return c.rend();
            }
            static IterType& move(IterType& i, int step = 1)
            {
                std::advance(i, step);
                return i;
            }
            static uint32 getSize(const ContainerType& conn)
            {
                return (uint32)conn.size();
            }
        };   
        template<typename ContainerType>      
        struct ContainerTraits<const ContainerType, false>
        {      
            typedef typename const ContainerType::value_type   ValueType;   
            typedef typename ContainerType::const_iterator     IterType;

            static IterType begin(const ContainerType& c)
            {
                return c.begin();
            }
            static IterType end(const ContainerType& c)
            {
                return c.end();
            }
            static IterType& move(IterType& i, int step = 1)
            {
                std::advance(i, step);
                return i;
            }
            static uint32 getSize(const ContainerType& conn)
            {
                return (uint32)conn.size();
            }
        };    
        template<typename ContainerType>      
        struct ContainerTraits<const ContainerType, true>
        {      
            typedef typename const ContainerType::value_type        ValueType;   
            typedef typename ContainerType::const_reverse_iterator  IterType;

            static IterType begin(const ContainerType& c)
            {
                return c.rbegin();
            }
            static IterType end(const ContainerType& c)
            {
                return c.rend();
            }
            static IterType& move(IterType& i, int step = 1)
            {
                std::advance(i, step);
                return i;
            }
            static uint32 getSize(const ContainerType& conn)
            {
                return (uint32)conn.size();
            }
        }; 

        template<typename T, int len>      
        struct ContainerTraits<T[len], false>      
        {      
            typedef T*  IterType;
            typedef T   ValueType;      

            static IterType begin(T (&a)[len])
            {
                return a;
            }
            static IterType end(T (&a)[len])
            {
                return a + len;
            }
            static IterType& move(IterType& i, int step = 1)
            {
                i += step;
                return i;
            }
            static uint32 getSize(T(&a)[len])
            {
                return len;
            }
        };   
        template<typename T, int len>      
        struct ContainerTraits<T[len], true>      
        {      
            typedef T*  IterType;
            typedef T   ValueType;      

            static IterType begin(T (&a)[len])
            {
                return a + len - 1;
            }
            static IterType end(T (&a)[len])
            {
                return a - 1;
            }
            static IterType& move(IterType& i, int step = 1)
            {
                i += -step;
                return i;
            }
            static uint32 getSize(T(&a)[len])
            {
                return len;
            }
        };  

        template<typename T, int len>      
        struct ContainerTraits<const T[len], false>      
        {      
            typedef const T*  IterType;
            typedef const T   ValueType;  

            static IterType begin(const T (&a)[len])
            {
                return a;
            }
            static IterType end(const T (&a)[len])
            {
                return a + len;
            }
            static IterType& move(IterType& i, int step = 1)
            {
                i += step;
                return i;
            }
            static uint32 getSize(T(&a)[len])
            {
                return len;
            }
        };  

        template<typename T, int len>      
        struct ContainerTraits<const T[len], true>      
        {      
            typedef const T*  IterType;
            typedef const T   ValueType;  

            static IterType begin(const T (&a)[len])
            {
                return a + len - 1;
            }
            static IterType end(const T (&a)[len])
            {
                return a - 1;
            }
            static IterType& move(IterType& i, int step = 1)
            {
                i += -step;
                return i;
            }
            static uint32 getSize(T(&a)[len])
            {
                return len;
            }
        }; 

        // 类型计数器, 辅助类
        template<int N> struct DefaultTypeCounter;
        /** 
            @brief 类型到数字的转化
            复杂度是currentTypeCount / max(#即256) + (currentTypeCount % max)->转化到4进制后的各位和
        */
        template<typename T, template<int> class CounterSet = DefaultTypeCounter, int n = 0>
        struct TypeToInt
        {
            __if_exists(CounterSet<n + 256>)
            {
                enum { value = TypeToInt<T, CounterSet, n + 257>::value };
            }
            __if_not_exists(CounterSet<n + 256>)
            {
                __if_exists(CounterSet<n + 64>)
                {
                    enum { value = TypeToInt<T, CounterSet, n + 65>::value };
                }
                __if_not_exists(CounterSet<n + 64>)
                {
                    __if_exists(CounterSet<n + 16>)
                    {
                        enum { value = TypeToInt<T, CounterSet, n + 17>::value };
                    }
                    __if_not_exists(CounterSet<n + 16>)
                    {
                        __if_exists(CounterSet<n + 4>)
                        {
                            enum { value = TypeToInt<T, CounterSet, n + 5>::value };
                        }
                        __if_not_exists(CounterSet<n + 4>)
                        {
                            __if_exists(CounterSet<n>)
                            {
                                enum { value = TypeToInt<T, CounterSet, n + 1>::value };
                            }
                            __if_not_exists(CounterSet<n>)
                            {
                                enum { value = n };
                                typedef CounterSet<n> Type;
                            }
                        }
                    }
                }
            }
        };

        // 由于产生了更多的类型, 下面的二分查找算法反而更慢
       /* template<int n>
        struct Counter;

        template<typename T, int min, int max>
        struct TypeToIntTween
        {
            __if_exists(Counter<min + 1>)
            {
                __if_exists(Counter<(min + max) / 2>)
                {
                    enum { value = TypeToIntTween<T, (min + max) / 2, max>::value };
                }
                __if_not_exists(Counter<(min + max) / 2>)
                {
                    enum { value = TypeToIntTween<T, min, (min + max) / 2>::value };
                }
            }
            __if_not_exists(Counter<min + 1>)
            {
                enum { value = min + 1 };   
                typedef Counter<min + 1> Type;
            }
        };

        template<typename T, int n = 1>
        struct TypeToInt
        {
            __if_exists(Counter<n>)
            {
                __if_exists(Counter<n * 2>)
                {
                    enum { value = TypeToInt<T, n * 2>::value };
                };
                __if_not_exists(Counter<n * 2>)
                {
                    enum { value = TypeToIntTween<T, n, n * 2>::value };
                }
            };
            __if_not_exists(Counter<n>)
            {
                enum { value = n };
                typedef Counter<n> Type;
            }
        };*/
    }
}