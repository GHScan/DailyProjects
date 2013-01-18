#pragma once

#include <boost/function.hpp>
#include <boost/bind.hpp>
#include <boost/scoped_ptr.hpp>

#include "Preprocessor.h"
#include "Types.h"

namespace Scan
{
    /**
        @brief �����βε�ί����
        ��boost::function��boost::bind�󶨵���һ��
    */
    template<typename ResultType = void>
    class Delegate0
    {
    public:
        ResultType operator () () const 
        { 
            return m_func(); 
        }

        Delegate0(){}

        template<typename FuncType>
        explicit Delegate0(FuncType f): m_func(f){}

        template<typename FuncType>
        Delegate0& bind(FuncType f)
        {
            m_func = f;
            return *this;
        }

#define _SCAN_DELEGATE0_CONSTRUCT_PARAM_N(n) \
    template<typename FuncType, SCAN_PP_REPEAT_TYPENAME_TYPE_COMMA(n)> \
    Delegate0(FuncType f, SCAN_PP_REPEAT_TYPE_PARAM_COMMA(n)): \
    m_func(boost::bind(f, SCAN_PP_REPEAT_PARAM_COMMA(n))){}

#define _SCAN_DELEGATE0_BIND_PARAM_N(n) \
    template<typename FuncType, SCAN_PP_REPEAT_TYPENAME_TYPE_COMMA(n)> \
    Delegate0& bind(FuncType f, SCAN_PP_REPEAT_TYPE_PARAM_COMMA(n)) \
        { \
        m_func = boost::bind(f, SCAN_PP_REPEAT_PARAM_COMMA(n)); \
        return *this; \
        }
        // boost::bindʵ�������9�������İ�
        SCAN_PP_REPEAT_LIMITED_DATA_EMPTY(1, 9, _SCAN_DELEGATE0_CONSTRUCT_PARAM_N);
        SCAN_PP_REPEAT_LIMITED_DATA_EMPTY(1, 9, _SCAN_DELEGATE0_BIND_PARAM_N);

#undef _SCAN_DELEGATE0_CONSTRUCT_PARAM_N
#undef _SCAN_DELEGATE0_BIND_PARAM_N

    private:
        boost::function<ResultType()>     m_func;
    };

    /**
        @brief ������βε�ί��
        �����βεĸ���ͨ��boost::function<void(int, int, ...)>��ָ��
    */
    template<typename FunctionType = boost::function<void()>>
    class DelegateN
    {
    private:
        typedef typename FunctionType::result_type   ResultType;

    public:
        ResultType operator () () const 
        { 
            return m_func(); 
        }

        DelegateN(){}

        template<typename FuncType>
        explicit DelegateN(FuncType f): m_func(f){}

        template<typename FuncType>
        DelegateN& bind(FuncType f)
        {
            m_func = f;
            return *this;
        }

#define SCAN_DELEGATEN_CONSTRUCT_PARAM_N(n) \
    template<typename FuncType, SCAN_PP_REPEAT_TYPENAME_TYPE_COMMA(n)> \
    DelegateN(FuncType f, SCAN_PP_REPEAT_TYPE_PARAM_COMMA(n)): \
    m_func(boost::bind(f, SCAN_PP_REPEAT_PARAM_COMMA(n))){}

#define SCAN_DELEGATEN_BIND_PARAM_N(n) \
    template<typename FuncType, SCAN_PP_REPEAT_TYPENAME_TYPE_COMMA(n)> \
    DelegateN& bind(FuncType f, SCAN_PP_REPEAT_TYPE_PARAM_COMMA(n)) \
        { \
        m_func = boost::bind(f, SCAN_PP_REPEAT_PARAM_COMMA(n)); \
        return *this; \
        }

#define SCAN_DELEGATEN_OPERATOR_PARAM_N(n) \
    template<SCAN_PP_REPEAT_TYPENAME_TYPE_COMMA(n)> \
    ResultType operator () (SCAN_PP_REPEAT_TYPE_PARAM_COMMA(n)) const \
        { \
        return m_func(SCAN_PP_REPEAT_PARAM_COMMA(n)); \
        }

        // boost::bindʵ�������9�������İ�
        SCAN_PP_REPEAT_LIMITED_DATA_EMPTY(1, 9, SCAN_DELEGATEN_CONSTRUCT_PARAM_N);
        SCAN_PP_REPEAT_LIMITED_DATA_EMPTY(1, 9, SCAN_DELEGATEN_BIND_PARAM_N);
        SCAN_PP_REPEAT_LIMITED_DATA_EMPTY(1, 9, SCAN_DELEGATEN_OPERATOR_PARAM_N);

#undef SCAN_DELEGATEN_CONSTRUCT_PARAM_N
#undef SCAN_DELEGATEN_BIND_PARAM_N
#undef SCAN_DELEGATEN_OPERATOR_PARAM_N

    private:
        FunctionType    m_func;
    };

    class DelegateThreadImpl;
    class DelegateThread;

    /**
        @brief ί���̹߳۲���
    */
    struct IDelegateThreadListener
    {
        /**
            @brief ί���߳̿�ʼִ��һ��ί��
            @param delegateId ί���¼��ı�־
        */
        virtual void onBeginCall(DelegateThread* td, void* delegateId){}

        /**
            @brief ί���߳̽���һ��ί�е�ִ��
        */
        virtual void onEndCall(DelegateThread* td, void* delegateId){}

        /**
            @brief ί���߳̽�������
        */
        virtual void onThreadSleep(DelegateThread *) {};

        /**
            @brief ί���߳�����
        */
        virtual void onThreadWakeup(DelegateThread *) {};

        virtual ~IDelegateThreadListener() = 0 {}
    };

    /**
        @brief ί���߳�
    */
    class DelegateThread
    {
    public:
        typedef Delegate0<> Delegate;
    public:
        DelegateThread();
        ~DelegateThread();

        /**
            @brief ��ί���߳�ִ��һ��ί���¼�
            @param id ���ί���¼��ı��
            @param priority �¼������ȼ�, 0��С
        */
        void addCall(const Delegate& d, void* id, uint32 priority = 0);
        void addListener(IDelegateThreadListener *p);
        void removeListener(IDelegateThreadListener *p);

    private:
        /**
            @brief ��ֹ����
        */
        DelegateThread(const DelegateThread&);
        DelegateThread& operator = (const DelegateThread&);

    private:
        boost::scoped_ptr<DelegateThreadImpl>  m_impl;
    };

    class DelegateThreadPoolImpl;
    class DelegateThreadPool;

    /**
        @brief ί���̳߳ع۲���
    */
    struct IDelegateThreadPoolListener
    {
        virtual ~IDelegateThreadPoolListener() = 0 {}

        /**
            @brief һ��ί�п�ʼִ��
        */
        virtual void onBeginCall(DelegateThreadPool *p, void* delegateID) {}

        /**
            @brief ί��ִ�н���
        */
        virtual void onEndCall(DelegateThreadPool *p, void* delegateID) {}
    };

    /**
        @brief ί���̳߳�
    */
    class DelegateThreadPool
    {
    public:
        typedef Delegate0<> Delegate;   
    public:
        /**
            @brief �̳߳������Զ����ӵ��߳���
            ��ʼ�߳����Ǵ�����������������
        */
        DelegateThreadPool(uint32 autoExtendCount = 0);
        ~DelegateThreadPool();

        /**
            @brief ���һ��Ҫִ�е�ί��
        */
        void addCall(const Delegate& d, void* id, uint32 priority = 0);
        void addListener(IDelegateThreadPoolListener *p);
        void removeListener(IDelegateThreadPoolListener *p);

    private:
        DelegateThreadPool(const DelegateThreadPool&);
        DelegateThreadPool& operator = (const DelegateThreadPool&);

    private:
        boost::scoped_ptr<DelegateThreadPoolImpl>  m_impl;
    };
}