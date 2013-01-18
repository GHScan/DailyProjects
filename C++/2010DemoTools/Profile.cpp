#include "StdAfx.h"

#include <functional>
#include <map>
#include <algorithm>

#include "Profile.h"
#include "PlatformDepends.h"
#include "Serialize.h"
#include "Factory.h"

#include "MemoryCheck.h"

namespace Scan
{
    namespace Profile
    {
        /**
            @brief 可串行化的函数调用节点
        */
        class SerializeableFuncNode:
            public Serialize::ISerializeable
        {
        public:
            /**
                @brief 从实际的节点树构造串行化节点树
            */
            void fromFuncNodes(const FuncNode *node)
            {
                m_name = node->name;
                m_costSeconds = node->costSeconds;
                for (FuncNodeList_CIter iter = node->childs.begin();
                    iter != node->childs.end();
                    ++iter)
                {
                    m_childs.push_back(SerializeableFuncNode());
                    m_childs.back().fromFuncNodes(*iter);
                }
            }

            /**
                @brief 从串行化节点树转化为实际节点树
            */
            void toFuncNodes(FuncNode *node) const
            {
                node->name = m_name;
                node->costSeconds = m_costSeconds;

                for (SerializeFuncNodeList_CIter iter = m_childs.begin();
                    iter != m_childs.end();
                    ++iter)
                {
                    node->childs.push_back(new FuncNode());
                    node->childs.back()->parent = node;
                    iter->toFuncNodes(node->childs.back());
                }
            }

            virtual void onSerialzie(Serialize::ISerializeFile& f)
            {
                f & SCAN_SERIALIZE_NVP("函数名", m_name);
                f & SCAN_SERIALIZE_NVP("耗时", m_costSeconds);
                f & SCAN_SERIALIZE_NVP("子调用", m_childs);
            }

        private:
            typedef std::vector<SerializeableFuncNode>      SerializeFuncNodeList;
            typedef SerializeFuncNodeList::const_iterator   SerializeFuncNodeList_CIter;

        private:
            String                  m_name;
            double                  m_costSeconds;
            SerializeFuncNodeList   m_childs;
        };

        /**
            @brief 函数调用图中的节点指针
        */
        struct IFuncGraphPos
        {
            virtual ~IFuncGraphPos() = 0 {}

            virtual FuncNode* getCurPos(FuncNode *defaultNode = NULL) = 0;
            virtual void setCurPos(FuncNode *node) = 0;
        };

        /**
            @brief 单线程节点指针
        */
        class STFuncGraphPos:
            public IFuncGraphPos
        {
        public:
            STFuncGraphPos():
              m_node(NULL)
            {
            }

            virtual FuncNode* getCurPos(FuncNode *defaultNode)
            {
                if (m_node == NULL)
                {
                    m_node = defaultNode;
                }
                return m_node;
            }

            virtual void setCurPos(FuncNode *node)
            {
                m_node = node;
            }

        private:
            FuncNode    *m_node;
        };

        /**
            @brief 多线程节点指针
        */
        class MTFuncGraphPos:
            public IFuncGraphPos
        {
        public:
            virtual FuncNode* getCurPos(FuncNode *defaultNode)
            {
                if (m_node.getValue() == NULL)
                {
                    m_node.setValue(defaultNode);
                }
                return m_node.getValue();
            }

            virtual void setCurPos(FuncNode *node)
            {
                m_node.setValue(node);
            }

        private:
            Windows::TlsDWORD<FuncNode*>    m_node;
        };

        /**
            @brief 性能测试器的实现
        */
        class ProfilerImpl
        {
        public:
            struct FuncNodePtrLess:
                public std::binary_function<FuncNode*, FuncNode*, bool>
            {
                bool operator () (const FuncNode* lhs, const FuncNode* rhs) const
                {
                    return lhs->name < rhs->name;
                }
            };

            struct FuncNodeEqual:
                public std::binary_function<FuncNode*, FuncNode*, bool>
            {
                bool operator () (const FuncNode *lhs, const FuncNode *rhs) const
                {
                    return lhs->name == rhs->name;
                }
            };

        public:
            ProfilerImpl(bool isMultiThread):
              m_isMultiThread(isMultiThread),
                  m_root("Root")
              {   
                  if (m_isMultiThread)
                  {
                      m_syncObj = new Windows::CriticalSection;
                      m_curGraphPos = new MTFuncGraphPos;
                  }
                  else
                  {
                      m_syncObj = new EmptyMutex;
                      m_curGraphPos = new STFuncGraphPos;
                  }
              }

              ~ProfilerImpl()
              {
                  cleanup();

                  safe_delete(m_curGraphPos);
                  safe_delete(m_syncObj);
              }

              bool load(const char *file, bool isXmlFormat)
              {
                  SingleLocker locker(m_syncObj);

                  Serialize::SerializeFilePtr f;
                  Factory::createObjectSptr(f, 
                      isXmlFormat ? 
                      getStaticClassName<Serialize::SerializeXmlFileIn>() : 
                  getStaticClassName<Serialize::SerializeBinaryFileIn>());

                  f->open(file);

                  if (f->isOpened())
                  {
                      SerializeableFuncNode sNods;
                      *f & SCAN_SERIALIZE_NVP("性能分析结果", sNods);

                      cleanup();
                      sNods.toFuncNodes(&m_root);

                      FuncNodeList list;
                      list = m_root.childs;
                      while (!list.empty())
                      {
                          FuncNode *node = list.back();
                          list.pop_back();
                          m_funcMap[node->name].push_back(node);
                          list.insert(list.end(), node->childs.begin(), node->childs.end());
                      }

                      return true;
                  }
                  else
                  {
                      return false;
                  }
              }

              bool save(const char *file, bool isXmlFormat) const
              {
                  SingleLocker locker(m_syncObj);

                  Serialize::SerializeFilePtr f;
                  Factory::createObjectSptr(f, 
                      isXmlFormat ? 
                      getStaticClassName<Serialize::SerializeXmlFileOut>() : 
                  getStaticClassName<Serialize::SerializeBinaryFileOut>());

                  f->open(file);

                  if (f->isOpened())            
                  {
                      SerializeableFuncNode sNodes;
                      sNodes.fromFuncNodes(&m_root);

                      *f & SCAN_SERIALIZE_NVP("性能分析结果", sNodes);

                      return true;
                  }
                  else
                  {
                      return false;
                  }
              }

              void getRootNodes(FuncNodeList& roots) const
              {
                  SingleLocker locker(m_syncObj);

                  roots = m_root.childs;
              }

              bool getNodesByName(const char *name, FuncNodeList &nodes) const
              {
                  SingleLocker locker(m_syncObj);

                  FuncMap_CIter iter = m_funcMap.find(name);
                  if (iter != m_funcMap.end())
                  {
                      nodes = iter->second;
                      return true;
                  }
                  return false;
              }

              void getAllFuncNames(StringVector& names) const
              {
                  SingleLocker locker(m_syncObj);

                  for (FuncMap_CIter iter = m_funcMap.begin();
                      iter != m_funcMap.end();
                      ++iter)
                  {
                      names.push_back(iter->first);
                  }
              }

              void reset()
              {
                  SingleLocker locker(m_syncObj);

                  FuncNodeList list;
                  list.push_back(&m_root);

                  while (!list.empty())
                  {
                      FuncNode *node = list.back();
                      list.pop_back();
                      node->costSeconds = 0;
                      list.insert(list.end(), node->childs.begin(), node->childs.end());
                  }
              }

              void _beginFunc(const char *name)
              {
                  SingleLocker locker(m_syncObj);

                  FuncNode *parent = m_curGraphPos->getCurPos(&m_root);
                  FuncNode curNode; 
                  curNode.name = name;
                  FuncNodeList_Iter iter = std::lower_bound(parent->childs.begin(), parent->childs.end(), &curNode, FuncNodePtrLess());
                  if (iter == parent->childs.end() || !FuncNodeEqual()(*iter, &curNode))
                  {
                      FuncNode *node = new FuncNode;
                      node->name = name;
                      node->parent = parent;
                      m_funcMap[name].push_back(node);
                      iter = parent->childs.insert(iter, node);
                  }
                  m_curGraphPos->setCurPos(*iter);
              }

              void _endFunc(double costSeconds)
              {
                  SingleLocker locker(m_syncObj);

                  FuncNode *node = m_curGraphPos->getCurPos(&m_root);
                  node->costSeconds += costSeconds;
                  m_curGraphPos->setCurPos(node->parent);
              }

        private:
            void cleanup()
            {
                FuncNodeList list = m_root.childs;

                while (!list.empty())
                {
                    FuncNode *node = list.back();    
                    list.pop_back();
                    list.insert(list.end(), node->childs.begin(), node->childs.end());
                    delete node;
                }

                m_root.childs.clear();
                m_funcMap.clear();
            }

        private:
            typedef std::map<String, FuncNodeList>  FuncMap;
            typedef FuncMap::value_type             FuncMap_Value;
            typedef FuncMap::iterator               FuncMap_Iter;
            typedef FuncMap::const_iterator         FuncMap_CIter;

        private:
            FuncMap                 m_funcMap;
            FuncNode                m_root;
            bool                    m_isMultiThread;
            mutable ISyncObject    *m_syncObj;
            IFuncGraphPos          *m_curGraphPos;
        };

        Profiler::Profiler(bool isMultiThread, const char *file):
        m_impl(new ProfilerImpl(isMultiThread))
        {
            if (file != NULL)
            {
                load(file);
            }
        }

        Profiler::~Profiler()
        {
            safe_delete(m_impl);
        }

        bool Profiler::load(const char *file, bool isXmlFormat)
        {
            return m_impl->load(file, isXmlFormat);
        }

        bool Profiler::save(const char *file, bool isXmlFormat) const
        {
            return m_impl->save(file, isXmlFormat);
        }

        void Profiler::getRootNodes(FuncNodeList &roots) const
        {
            m_impl->getRootNodes(roots);
        }

        bool Profiler::getNodesByFuncName(const char *name, FuncNodeList &nodes) const
        {
            return m_impl->getNodesByName(name, nodes);
        }

        void Profiler::getAllFuncNames(StringVector& names) const
        {
            m_impl->getAllFuncNames(names);
        }

        void Profiler::reset()
        {
            m_impl->reset();
        }

        void Profiler::_beginFunc(const char *name)
        {
            m_impl->_beginFunc(name);
        }

        void Profiler::_endFunc(double costSeconds)
        {
            m_impl->_endFunc(costSeconds);
        }

        Profiler *getDefaultProfiler()
        {
            static Profiler ls_profiler(true);
            return &ls_profiler;
        }
    }
}

SCAN_FACTORY_REGISTER(Scan::Serialize::ISerializeable, Scan::Profile::SerializeableFuncNode)