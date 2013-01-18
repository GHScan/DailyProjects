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
            @brief ��ʾһ��������һ���������еĵ���
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
            double          costSeconds; /**< ��������ۼƻ��ѵ�ʱ�� */
            FuncNode *      parent; /**< �������ĺ��� */
            FuncNodeList    childs; /**< Ҫ���õ��Ӻ��� */
        };

        class ProfilerImpl;
        class Profiler
        {
        public:
            /**
                @brief ����
                @param isMultiThread �Ƿ��ڶ��̻߳�����ʹ��
                @param file ��NULL��ʾ���ļ��м�����ǰ�����ܲ�������
            */
            Profiler(bool isMultiThread = false, const char *file = NULL);
            ~Profiler();

            /**
                @brief ���ļ��м������ܲ�������
                @param isXmlFormat �ļ���xml��ʽ���߶������ļ�
            */
            bool load(const char *file, bool isXmlFormat = true);
            bool save(const char *file, bool isXmlFormat = true) const;

            /**
                @brief ��ȡ�������ܲ��Եĸ�������
                @param roots û�и����û��߸����õ�û�в������ܲ��Ե���Щ����
            */
            void getRootNodes(FuncNodeList& roots) const;

            /**
                @brief ����һ��������ѯ�������е��õ�Ϳ���
                @param nodes ÿ�����õ�Ϳ���
            */
            bool getNodesByFuncName(const char *name, FuncNodeList &nodes) const;

            /**
                @brief ��ȡ���в������ܲ��Եĺ�������
            */
            void getAllFuncNames(StringVector& names) const;

            /**
                @brief �������ܲ��Ե�ͳ��
            */
            void reset();

            /**
                @brief �ڲ�ʹ��; ֪ͨһ��������ʼ
                @param name ������
            */
            void _beginFunc(const char *name);
            
            /**
                @brief �ڲ�ʹ��; ֪ͨ���һ���������ý�����
            */
            void _endFunc(double costSeconds);

        private:
            Profiler(const Profiler&);
            Profiler& operator = (const Profiler&);

        private:
            ProfilerImpl    *m_impl;
        };

        /**
            @brief �������ܲ��Ը�����
        */
        class FuncTimer
        {
        public:
            /**
                @brief ����; ���һ���������õĿ�ʼ
            */
            FuncTimer(const char *funcName, Profiler *parent):
              m_beginTimer(getInstructionCount()), m_parent(parent)
              {
                  m_parent->_beginFunc(funcName);
              }

            /**
                @brief ����; ��¼�������ú�ʱ
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
            @brief Ĭ�ϵ����ܲ��Ի�;���߳�
        */
        Profiler *getDefaultProfiler();

        /**
            @brief ��ʼͳ��һ���������õ�����

            �����ڴ��ⲿͳ��;��:{ PROFILE_HOOK("Sleep"); Sleep(10); }

            @param name ָ��������
        */
#define PROFILE_HOOK(name)  Scan::Profile::FuncTimer SCAN_PP_CAT(__funcTimer, __LINE__)(name, Scan::Profile::getDefaultProfiler())

        /**
            @brief ��ʼͳ��һ���������õ�����

            �����ڴӺ����ڲ�����ͳ��; �� void fun() { PROFILE_HOOK_AUTO(); ...}
        */
#define PROFILE_HOOK_AUTO() PROFILE_HOOK(SCAN_FUNCTION)
    }
}