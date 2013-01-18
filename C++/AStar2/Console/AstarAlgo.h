#pragma once

#include <vector>

#include "min_heap.h"
#include "ordered_vector.h"

struct IAstarMapProvider
{
    virtual ~IAstarMapProvider(){}

    struct NodeT{};

    virtual NodeT* getSrcNode() = 0;
    virtual NodeT* getDestNode() = 0;

    virtual void setParent(NodeT* child, NodeT* parent) = 0;
    virtual NodeT* getParent(NodeT*) = 0;

    virtual void resetScore(NodeT*, bool cost = true, bool distance = true) = 0;
    virtual bool isPathNearer(NodeT* testNode, NodeT *newParent) = 0;

    virtual void maskAsPath(NodeT*) = 0;
    virtual void maskAsFound(NodeT*) = 0;

    virtual std::vector<NodeT*>& getAdjacencyNodes(NodeT*) = 0;

    // 适配接口
    virtual bool scoreLess(NodeT*, NodeT*) = 0;
    virtual void setUserData(NodeT*, size_t) = 0;
    virtual size_t getUserData(NodeT*) = 0;
    virtual NodeT* getMinimumScoreNode() = 0;
};

template<>
struct min_heap_traits<IAstarMapProvider::NodeT*>
{
    typedef IAstarMapProvider::NodeT    NodeT;
    typedef std::vector<NodeT*>	        VecT;

    min_heap_traits(IAstarMapProvider* provider):m_provider(provider){}

    size_t get_heap_pos(NodeT* const& val){ return m_provider->getUserData(val); }
    void set_heap_pos(NodeT*& val, size_t pos){ m_provider->setUserData(val, pos); }
    bool compare(NodeT* const& lhs, NodeT*const& rhs){ return m_provider->scoreLess(lhs, rhs); }
    NodeT* const minimum() { return m_provider->getMinimumScoreNode(); }

private:
    IAstarMapProvider   *m_provider;
};

bool doAstarAlgo(IAstarMapProvider* provider)
{
    typedef IAstarMapProvider::NodeT    NodeT;

    ordered_vector<false, NodeT*>   openSet;
    ordered_vector<false, NodeT*>   closedSet;
    min_heap<NodeT*> openHeap((min_heap_traits<NodeT*>(provider)));

    assert(provider->getSrcNode() != provider->getDestNode());

    NodeT *src = provider->getSrcNode();
    provider->setParent(src, NULL);
    provider->resetScore(src);
    openSet.insert(src);
    openHeap.push(src);

    NodeT *lastNode = NULL;

    while (lastNode == NULL && !openSet.empty())
    {
        NodeT* minScoreNode = openHeap.top();
        openHeap.pop();
        openSet.erase(minScoreNode);
        closedSet.insert(minScoreNode);

        std::vector<NodeT*>& adjacencyNodes = provider->getAdjacencyNodes(minScoreNode);
        for (size_t i = 0; i < adjacencyNodes.size(); ++i)
        {
            NodeT* node = adjacencyNodes[i];

            if (node == provider->getDestNode())
            {
                provider->setParent(node, minScoreNode);
                lastNode = node;
                break;
            }

            // 死亡节点
            if (closedSet.count(node) > 0) continue;

            {
                // 活跃节点, 尝试以当前节点作为父亲
                ordered_vector<false, NodeT*>::iterator iter = openSet.find(node);
                if (iter != openSet.end())
                {
                    if (provider->isPathNearer(node, minScoreNode))
                    {
                        openHeap.erase(node);
                        provider->setParent(node, minScoreNode);
                        provider->resetScore(node, true, false);
                        openHeap.push(node);
                    }

                    continue;
                }
            }

            provider->setParent(node, minScoreNode);
            provider->resetScore(node);
            openSet.insert(node);
            openHeap.push(node);
        }
    }

    if (lastNode == NULL) return false;

    assert(lastNode == provider->getDestNode());
    lastNode = provider->getParent(lastNode);
    while (NodeT* parent = provider->getParent(lastNode))
    {
        provider->maskAsPath(lastNode);
        lastNode = parent;
    }
    assert(lastNode == provider->getSrcNode());

    {
        ordered_vector<false, NodeT*>::iterator iter = openSet.begin();
        for (; iter != openSet.end(); ++iter) provider->maskAsFound(*iter);
        iter = closedSet.begin();
        for (; iter != closedSet.end(); ++iter) provider->maskAsFound(*iter);
    }

    return true;
};  