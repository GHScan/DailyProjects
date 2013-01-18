#pragma once

#include <vector>

#include "Types.h"
#include "PlatformDepends.h"
#include "CompileEnvironment.h"
#include "Preprocessor.h"

namespace Scan
{
    namespace Profile
    {
        struct FuncNode;
        typedef std::vector<FuncNode*>          FuncNodeList;
        typedef FuncNodeList::iterator          FuncNodeList_Iter;
        typedef FuncNodeList::const_iterator    FuncNodeList_CIter;

        /**
            @brief 表示一个函数在一个上下文中的调用
        */
        struct FuncNode
        {
            FuncNode(const char *name = "", FuncNode *parent = NULL):
            costSeconds(0)
            {
                this->name = name; 
                this->parent = parent;
            }
        
            String          name; 
            double          costSeconds; /**< 这个调用累计花费的时间 */
            FuncNode *      parent; /**< 调用它的函数 */
            FuncNodeList    childs; /**< 要调用的子函数 */
        };

        class ProfilerImpl;
        class Profiler
        {
        public:
            /**
                @brief 构造
                @param isMultiThread 是否在多线程环境中使用
                @param file 非NULL表示从文件中加载以前的性能测试数据
            */
            Profiler(bool isMultiThread = false, const char *file = NULL);
            ~Profiler();

            /**
                @brief 从文件中加载性能测试数据
                @param isXmlFormat 文件是xml格式或者二进制文件
            */
            bool load(const char *file, bool isXmlFormat = true);
            bool save(const char *file, bool isXmlFormat = true) const;

            /**
                @brief 获取参与性能测试的根函数集
                @param roots 没有父调用或者父调用的没有参与性能测试的那些函数
            */
            void getRootNodes(FuncNodeList& roots) const;

            /**
                @brief 根据一个函数查询它的所有调用点和开销
                @param nodes 每个调用点和开销
            */
            bool getNodesByFuncName(const char *name, FuncNodeList &nodes) const;

            /**
                @brief 获取所有参与性能测试的函数名称
            */
            void getAllFuncNames(StringVector& names) const;

            /**
                @brief 重置性能测试的统计
            */
            void reset();

            /**
                @brief 内部使用; 通知一个函数开始
                @param name 函数名
            */
            void _beginFunc(const char *name);
            
            /**
                @brief 内部使用; 通知最近一个函数调用结束了
            */
            void _endFunc(double costSeconds);

        private:
            Profiler(const Profiler&);
            Profiler& operator = (const Profiler&);

        private:
            ProfilerImpl    *m_impl;
        };

        /**
            @brief 函数性能测试辅助类
        */
        class FuncTimer
        {
        public:
            /**
                @brief 构造; 标记一个函数调用的开始
            */
            FuncTimer(const char *funcName, Profiler *parent):
              m_beginTimer(getInstructionCount()), m_parent(parent)
              {
                  m_parent->_beginFunc(funcName);
              }

            /**
                @brief 析构; 记录函数调用耗时
            */
            ~FuncTimer()
            {
                m_parent->_endFunc(calcElapseTime(m_beginTimer));
            }

        private:
            FuncTimer(const FuncTimer&);
            FuncTimer& operator = (const FuncTimer&);

        private:
            Profiler    *m_parent;
            uint64       m_beginTimer;
        };

        /**
            @brief 默认的性能测试机;单线程
        */
        Profiler *getDefaultProfiler();

        /**
            @brief 开始统计一个函数调用的性能

            常用于从外部统计;如:{ PROFILE_HOOK("Sleep"); Sleep(10); }

            @param name 指定函数名
        */
#define PROFILE_HOOK(name)  Scan::Profile::FuncTimer SCAN_PP_CAT(__funcTimer, __LINE__)(name, Scan::Profile::getDefaultProfiler())

        /**
            @brief 开始统计一个函数调用的性能

            常用于从函数内部进行统计; 如 void fun() { PROFILE_HOOK_AUTO(); ...}
        */
#define PROFILE_HOOK_AUTO() PROFILE_HOOK(SCAN_FUNCTION)
    }
}