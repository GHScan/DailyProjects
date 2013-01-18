#pragma once

#include <vector>
#include <list>
#include <deque>
#include <set>
#include <map>

#include <boost/type_traits.hpp>

#include "MetaProgram.h"
#include "MemoryAllocation.h"
#include "Preprocessor.h"

namespace Scan
{
    namespace Serialize
    {
        /**
            @brief �ػ�һ������ģ��, ˵����stl��������
        */
        template<typename RawType>
        struct IsStlStyleContainer
        {
            static const bool value = false;
        };

#define SCAN_ADD_TO_STL_CONTAINER_LIST_TYPE_N(conType, n)               \
        template<SCAN_PP_REPEAT_TYPENAME_TYPE_COMMA(n)>                    \
        struct IsStlStyleContainer<conType<SCAN_PP_REPEAT_TYPE_COMMA(n)>>  \
        {                                                               \
            static const bool value = true;                             \
        };

        SCAN_ADD_TO_STL_CONTAINER_LIST_TYPE_N(std::vector, 2);
        SCAN_ADD_TO_STL_CONTAINER_LIST_TYPE_N(std::list, 2);
        SCAN_ADD_TO_STL_CONTAINER_LIST_TYPE_N(std::deque, 2);
        SCAN_ADD_TO_STL_CONTAINER_LIST_TYPE_N(std::set, 3);
        SCAN_ADD_TO_STL_CONTAINER_LIST_TYPE_N(std::map, 4);

        /**
            @brief �ж�һ�������Ƿ��Ǵ��л�֧�ֵ�����
        */
        template<typename ClassType>
        struct IsContainer
        {
            typedef typename boost::remove_cv<ClassType>::type  RawType;

            static const bool value = 
                IsStlStyleContainer<RawType>::value || 
                boost::is_array<RawType>::value;
        };


        template<typename ContainerType>
        class ContainerAdapter{};

        template<typename ContainerType>
        struct ContainerApaterImpl_Sequential_NeverInvalideIter{};

        /**
            @brief ���������
        */
        template<typename ValueType, int len>
        struct ContainerApaterImpl_Sequential_NeverInvalideIter<ValueType[len]>
        {
        public:
            typedef ValueType (Container)[len];

            ContainerApaterImpl_Sequential_NeverInvalideIter(Container& v): m_v(v), m_index(-1){}
            void operator ++ () { ++ m_index; }
            ValueType& operator () () { return m_v[m_index]; }
            void insertBehind() {  }

        private:
            Container  &m_v;
            uint32      m_index;
        };

        /**
            @brief ����ʧЧ��std::vector������
        */
        template<typename Type1, typename Type2>
        struct ContainerApaterImpl_Sequential_NeverInvalideIter<std::vector<Type1, Type2>>
        {
        public:
            typedef std::vector<Type1, Type2>  Container;
            typedef typename Container::value_type Value;

            ContainerApaterImpl_Sequential_NeverInvalideIter(Container& v): m_v(v), m_index(-1){}
            void operator ++ () { ++ m_index; }
            Value& operator () () { return m_v[m_index]; }
            void insertBehind() { m_v.push_back(Value()); }

        private:
            Container  &m_v;
            uint32      m_index;
        };

        /**
            @brief ����ʧЧ��std::deque������
        */
        template<typename Type1, typename Type2>
        struct ContainerApaterImpl_Sequential_NeverInvalideIter<std::deque<Type1, Type2>>
        {
        public:
            typedef std::deque<Type1, Type2>  Container;
            typedef typename Container::value_type Value;

            ContainerApaterImpl_Sequential_NeverInvalideIter(Container& v): m_v(v), m_index(-1){}
            void operator ++ () { ++ m_index; }
            Value& operator () () { return m_v[m_index]; }
            void insertBehind() { m_v.push_back(Value()); }

        private:
            Container  &m_v;
            uint32      m_index;
        };

        /**
            @brief std::list������
        */
        template<typename Type1, typename Type2>
        struct ContainerApaterImpl_Sequential_NeverInvalideIter<std::list<Type1, Type2>>
        {
        public:
            typedef std::list<Type1, Type2>  Container;
            typedef typename Container::iterator   Container_Iter;
            typedef typename Container::value_type Container_Value;

            ContainerApaterImpl_Sequential_NeverInvalideIter(Container& v): m_v(v), m_iter(v.end()){}
            void operator ++ () 
            {
                if (m_iter == m_v.end())
                {
                    m_iter = m_v.begin();
                }
                else
                {
                    ++m_iter;
                }
            }
            Container_Value& operator () () { return *m_iter; }
            void insertBehind() { m_v.push_back(Container_Value()); }

        private:
            Container       &m_v;
            Container_Iter   m_iter;
        };

        /**
            @brief ���л�����ʵ��
        */
        template<typename ContainerType>
        class ContainerApaterImpl_Sequential
        {
        public:
            ContainerApaterImpl_Sequential(ContainerType& conn):
              m_conn(conn), m_iter(conn){}

              typedef MPL::ContainerTraits<ContainerType, false>    ContainerTraits;

              void moveNext()
              {
                  ++m_iter;
              }

              void createNew()
              {
                  m_iter.insertBehind();
              }

              uint32 getSize() const
              {
                  return (uint32)ContainerTraits::getSize(m_conn);
              }

              typename ContainerTraits::ValueType& getCurrentItem() 
              {
                  return m_iter();
              }

        private:
            typedef ContainerApaterImpl_Sequential_NeverInvalideIter<ContainerType> Iter;
            Iter                m_iter;
            ContainerType      &m_conn;
        };

        /**
            @brief �����������������ػ�
        */
        template<typename ValueType, int len>
        class ContainerAdapter<ValueType[len]>:
            public MemoryAllocation::SizefixedMemAllocObject<ContainerAdapter<ValueType[len]>>
        {
        public:
            typedef ValueType  (ContainerType)[len];

            ContainerAdapter(ContainerType& v):
            m_impl(v){}
            void moveNext(){  m_impl.moveNext();}
            void createNew(){ m_impl.createNew(); }
            uint32 getSize() const { return m_impl.getSize(); }
            ValueType& getCurrentItem() { return m_impl.getCurrentItem(); }

        private:
            ContainerApaterImpl_Sequential<ContainerType> m_impl;
        };

        /**
            @brief ���л��������������ػ�
        */
#define SCAN_SERIALIZE_CONTAINERADAPTERIMPL_SEQUENTIAL_TYPE_N(conn, n)                              \
        template<SCAN_PP_REPEAT_TYPENAME_TYPE_COMMA(n)>                                                \
        class ContainerAdapter<conn<SCAN_PP_REPEAT_TYPE_COMMA(n)>>:                                    \
        public MemoryAllocation::SizefixedMemAllocObject<ContainerAdapter<conn<SCAN_PP_REPEAT_TYPE_COMMA(n)>>>    \
        {                                                                                           \
        public:                                                                                     \
        typedef conn<SCAN_PP_REPEAT_TYPE_COMMA(n)>  ContainerType;                                     \
        typedef typename ContainerType::value_type ValueType;                                       \
        ContainerAdapter(ContainerType& v):                                                         \
        m_impl(v){}                                                                                 \
        void moveNext(){  m_impl.moveNext();}                                                       \
        void createNew(){ m_impl.createNew(); }                                                     \
        uint32 getSize() const { return m_impl.getSize(); }                                         \
        ValueType& getCurrentItem() { return m_impl.getCurrentItem(); }                             \
        private:                                                                                    \
        ContainerApaterImpl_Sequential<ContainerType> m_impl;                                       \
        };

        SCAN_SERIALIZE_CONTAINERADAPTERIMPL_SEQUENTIAL_TYPE_N(std::vector, 2);
        SCAN_SERIALIZE_CONTAINERADAPTERIMPL_SEQUENTIAL_TYPE_N(std::deque, 2);
        SCAN_SERIALIZE_CONTAINERADAPTERIMPL_SEQUENTIAL_TYPE_N(std::list, 2);

        /**
            @brief ��ʼ��ָ��
            ������Ϊ���л��ļ�����ָ���ʱ������⴦��, ���������ļ�������ָ���
            newһ������, ���Կ�ָ��Ӧ����0
        */
        template<typename T>
        static void initVaule(T& v,
            typename boost::enable_if<boost::is_pointer<T>>::type* = 0)
        {
            toZero(v);
        }
        template<typename T>
        static void initVaule(T& v,
            typename boost::disable_if<boost::is_pointer<T>>::type* = 0)
        {

        }

        /**
            @brief ����������ʵ��
        */
        template<typename ContainerType>
        class ContainerAdapterImpl_Accociative
        {
        public:
            typedef ContainerType                    Container;
            typedef typename Container::value_type   Container_Value;
            typedef typename Container::iterator     Container_Iter;

            ContainerAdapterImpl_Accociative(Container& conn):
            m_conn(conn),
            m_isInputFile(false),
            m_inputTemp(NULL)
            {
                m_outputIter = m_conn.end();
            }

            ~ContainerAdapterImpl_Accociative()
            {
                if (m_inputTemp != NULL)
                {
                    m_conn.insert(*m_inputTemp);
                    delete m_inputTemp;
                }
            }

            void moveNext()
            {
                if (m_isInputFile)
                {
                    if (m_inputTemp == NULL)
                    {
                        m_inputTemp = new Container_Value;
                    }
                    else
                    {
                        m_conn.insert(*m_inputTemp);
                    }
                    initVaule(*m_inputTemp);
                }
                else
                {
                    if (m_outputIter == m_conn.end())
                    {
                        m_outputIter = m_conn.begin();
                    }
                    else
                    {
                        ++m_outputIter;
                    }
                }
            }

            void createNew()
            {
                m_isInputFile = true;
            }

            uint32 getSize() const
            {
                return (uint32)m_conn.size();
            }

            Container_Value& getCurrentItem() 
            {
                return m_isInputFile ? *m_inputTemp : *m_outputIter;
            }

        private:
            bool              m_isInputFile;
            Container_Value  *m_inputTemp;
            Container_Iter    m_outputIter;
            Container&        m_conn;
        };

        /**
            @brief �����������������ػ�
        */
#define SCAN_SERIALIZE_CONTAINERADAPTERIMPL_ACCOCIATIVE_TYPE_N(conn, n)                             \
        template<SCAN_PP_REPEAT_TYPENAME_TYPE_COMMA(n)>                                                \
        class ContainerAdapter<conn<SCAN_PP_REPEAT_TYPE_COMMA(n)>>:                                    \
            public MemoryAllocation::SizefixedMemAllocObject<ContainerAdapter<conn<SCAN_PP_REPEAT_TYPE_COMMA(n)>>>\
        {                                                                                           \
        public:                                                                                     \
            typedef conn<SCAN_PP_REPEAT_TYPE_COMMA(n)>     ContainerType;                              \
            typedef typename ContainerType::value_type  ValueType;                                  \
            ContainerAdapter(ContainerType& c): m_impl(c){}                                         \
            void moveNext() { m_impl.moveNext(); }                                                  \
            void createNew() { m_impl.createNew(); }                                                \
            uint32 getSize() const { return m_impl.getSize(); }                                     \
            ValueType& getCurrentItem() { return m_impl.getCurrentItem(); }                         \
        private:                                                                                    \
            ContainerAdapterImpl_Accociative<ContainerType> m_impl;                                 \
        };

        SCAN_SERIALIZE_CONTAINERADAPTERIMPL_ACCOCIATIVE_TYPE_N(std::set, 3);
        SCAN_SERIALIZE_CONTAINERADAPTERIMPL_ACCOCIATIVE_TYPE_N(std::map, 4);
    }
}