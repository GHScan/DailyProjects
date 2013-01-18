#pragma once

#include <boost/function.hpp>
#include <boost/bind.hpp>
#include <boost/scoped_ptr.hpp>

#include "Preprocessor.h"
#include "Types.h"

namespace Scan
{
    /**
        @brief 不含形参的委托类
        将boost::function和boost::bind绑定到了一起
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
        // boost::bind实现了最多9个参数的绑定
        SCAN_PP_REPEAT_LIMITED_DATA_EMPTY(1, 9, _SCAN_DELEGATE0_CONSTRUCT_PARAM_N);
        SCAN_PP_REPEAT_LIMITED_DATA_EMPTY(1, 9, _SCAN_DELEGATE0_BIND_PARAM_N);

#undef _SCAN_DELEGATE0_CONSTRUCT_PARAM_N
#undef _SCAN_DELEGATE0_BIND_PARAM_N

    private:
        boost::function<ResultType()>     m_func;
    };

    /**
        @brief 含多个形参的委托
        具体形参的个数通过boost::function<void(int, int, ...)>来指定
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

        // boost::bind实现了最多9个参数的绑定
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
        @brief 委托线程观察者
    */
    struct IDelegateThreadListener
    {
        /**
            @brief 委托线程开始执行一个委托
            @param delegateId 委托事件的标志
        */
        virtual void onBeginCall(DelegateThread* td, void* delegateId){}

        /**
            @brief 委托线程结束一个委托的执行
        */
        virtual void onEndCall(DelegateThread* td, void* delegateId){}

        /**
            @brief 委托线程进入休眠
        */
        virtual void onThreadSleep(DelegateThread *) {};

        /**
            @brief 委托线程醒来
        */
        virtual void onThreadWakeup(DelegateThread *) {};

        virtual ~IDelegateThreadListener() = 0 {}
    };

    /**
        @brief 委托线程
    */
    class DelegateThread
    {
    public:
        typedef Delegate0<> Delegate;
    public:
        DelegateThread();
        ~DelegateThread();

        /**
            @brief 让委托线程执行一个委托事件
            @param id 这个委托事件的标记
            @param priority 事件的优先级, 0最小
        */
        void addCall(const Delegate& d, void* id, uint32 priority = 0);
        void addListener(IDelegateThreadListener *p);
        void removeListener(IDelegateThreadListener *p);

    private:
        /**
            @brief 禁止拷贝
        */
        DelegateThread(const DelegateThread&);
        DelegateThread& operator = (const DelegateThread&);

    private:
        boost::scoped_ptr<DelegateThreadImpl>  m_impl;
    };

    class DelegateThreadPoolImpl;
    class DelegateThreadPool;

    /**
        @brief 委托线程池观察者
    */
    struct IDelegateThreadPoolListener
    {
        virtual ~IDelegateThreadPoolListener() = 0 {}

        /**
            @brief 一个委托开始执行
        */
        virtual void onBeginCall(DelegateThreadPool *p, void* delegateID) {}

        /**
            @brief 委托执行结束
        */
        virtual void onEndCall(DelegateThreadPool *p, void* delegateID) {}
    };

    /**
        @brief 委托线程池
    */
    class DelegateThreadPool
    {
    public:
        typedef Delegate0<> Delegate;   
    public:
        /**
            @brief 线程池允许自动增加的线程数
            初始线程数是处理器核心数的两倍
        */
        DelegateThreadPool(uint32 autoExtendCount = 0);
        ~DelegateThreadPool();

        /**
            @brief 添加一个要执行的委托
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