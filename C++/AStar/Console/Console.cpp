// Console.cpp : 定义控制台应用程序的入口点。
//

#include "stdafx.h"

#include <boost/pool/pool.hpp>
#include <boost/progress.hpp>

#include <cmath>
#include <cassert>

#include <set>
#include <vector>

const int MAP_SIZE = 800;
const int GRID_SIZE = 20;
const int GRID_W_CNT = (MAP_SIZE / GRID_SIZE);

enum GridType
{
    GT_Barrier = 1,
    GT_SrcDest,
    GT_Path,
    GT_Searched,
};

int         g_grid[GRID_W_CNT][GRID_W_CNT];

struct Node
{
    Node(int i, int j): pos((i << 16) | j), cost(0), distance(0), parent(NULL){}
    Node   *parent;
    uint    pos;
    ushort  cost;
    ushort  distance;

    void calcCostDistance(int destI, int destJ)
    {
        calcCost();
        distance = (abs(destI - (int)(pos >> 16)) + abs(destJ - (int)(pos & 0xffff))) * 2;
    }

    void setPos(int i, int j)
    {
        pos = (i << 16) | j;
    }

    void getPos(int &i, int &j)
    {
        i = pos >> 16;
        j = pos & 0xffff;
    }

    void calcCost()
    {
        cost = 0;
        Node *p = parent;
        while (p != NULL)
        {
            ++cost;
            p = p->parent;
        }
    }

    static void* operator new (size_t sz)
    {
        return getNodeMemPool()->malloc();
    }

    static void operator delete (void *p)
    {
        getNodeMemPool()->free(p);
    }

    static boost::pool<>* getNodeMemPool()
    {
        static boost::pool<> ls_pool(sizeof(Node));
        return &ls_pool;
    }
};

struct NodeLess_CostDistance
{
    bool operator() (const Node* lhs, const Node* rhs) const
    {
        return lhs->cost + lhs->distance < rhs->cost + rhs->distance;
    }
};

struct NodeLess_Pos
{
    bool operator() (const Node* lhs, const Node* rhs) const
    {
        return lhs->pos < rhs->pos;
    }
};

typedef std::set<Node*, NodeLess_Pos>                   NodeSet_Pos;
typedef std::multiset<Node*, NodeLess_CostDistance>     NodeSet_CostDistance;
typedef std::vector<Node*>                              NodeVec;

void doAstarWayFinding(int srcI, int srcJ, int destI, int destJ)
{
    boost::progress_timer _timer;

    assert(srcI != destI || srcJ != destJ);

    // 清除上次路径
    for (int i = 0; i < GRID_W_CNT; ++i)
    {
        for (int j = 0; j < GRID_W_CNT; ++j)
        {
            if (g_grid[i][j] == GT_Path || g_grid[i][j] == GT_Searched) g_grid[i][j] = 0;
        }
    }

    NodeSet_CostDistance    minCostDisOpenSet;
    NodeSet_Pos             queryOpenSet;
    NodeSet_Pos             queryCloseSet;
    NodeVec                 allocedNodes;
    Node                    queryNode(0, 0);
    Node                   *endNode = NULL;

    allocedNodes.push_back(new Node(srcI, srcJ));
    allocedNodes.back()->calcCostDistance(destI, destJ);
    minCostDisOpenSet.insert(allocedNodes.back());
    queryOpenSet.insert(allocedNodes.back());

    while (endNode == NULL && !minCostDisOpenSet.empty())
    {
        Node *minNode = *minCostDisOpenSet.begin();
        minCostDisOpenSet.erase(minCostDisOpenSet.begin());

        queryOpenSet.erase(minNode);
        queryCloseSet.insert(minNode);

        int i, j;
        minNode->getPos(i, j);
        for (int _i = i - 1; _i <= i + 1; ++_i)
        {
            for (int _j = j - 1; _j <= j + 1; ++_j)
            {
                if (_i != i && _j != j) continue;
                if (_i == i && _j == j) continue;
                if (_i < 0 || _i >= GRID_W_CNT) continue;
                if (_j < 0 || _j >= GRID_W_CNT) continue;

                // 如果是障碍, 直接跳过
                if (g_grid[_i][_j] == GT_Barrier) continue;

                queryNode.setPos(_i, _j);

                // 死掉了, 直接跳过
                if (queryCloseSet.count(&queryNode) == 1) continue;

                // 或者, 尝试更新
                NodeSet_Pos::iterator iter = queryOpenSet.find(&queryNode);
                if (iter != queryOpenSet.end())
                {
                    Node *node = *iter;
                    if (node->parent != minNode)
                    {
                        Node *oldParent = node->parent;
                        ushort oldCost = node->cost;
                        node->parent = minNode;
                        node->calcCost();
                        // 开销减小了, 需要更新位置
                        if (node->cost < oldCost)
                        {
                            std::swap(node->cost, oldCost);

                            queryNode.cost = node->cost;
                            queryNode.distance = node->distance;

                            NodeSet_CostDistance::_Pairii iterRange = minCostDisOpenSet.equal_range(&queryNode);
                            while (iterRange.first != iterRange.second)
                            {
                                if (*iterRange.first == node) break;
                                ++iterRange.first;
                            }

                            // 重新插入
                            if (iterRange.first != iterRange.second)
                            {
                                minCostDisOpenSet.erase(iterRange.first);
                                std::swap(node->cost, oldCost);
                                minCostDisOpenSet.insert(node);
                            }
                            else assert(0);
                        }
                        else
                        {
                            node->parent = oldParent;
                            node->cost = oldCost;
                        }
                    }

                    continue;
                }

                // 添加新节点
                allocedNodes.push_back(new Node(_i, _j));
                allocedNodes.back()->parent = minNode;

                if (_i == destI && _j == destJ)
                {
                    endNode = allocedNodes.back();
                    goto __succ;
                }

                allocedNodes.back()->calcCostDistance(destI, destJ);

                queryOpenSet.insert(allocedNodes.back());
                minCostDisOpenSet.insert(allocedNodes.back());
            }
        }
    }

__succ:
    if (endNode != NULL)
    {
        Node *p = endNode->parent;
        while (p->parent != NULL)
        {
            int i, j;
            p->getPos(i, j);
            assert(g_grid[i][j] == 0);
            g_grid[i][j] = GT_Path;

            p = p->parent;
        }
    }

    for (size_t k = 0; k < allocedNodes.size(); ++k)
    {
        Node *node = allocedNodes[k];
        int i, j;
        node->getPos(i, j);
        if (g_grid[i][j] == 0) g_grid[i][j] = GT_Searched;
    }

    while (!allocedNodes.empty())
    {
        delete allocedNodes.back();
        allocedNodes.pop_back();
    }
}

class MyLabel:
    public QLabel
{
public:
    MyLabel():
      m_barrierBrush(Qt::red), m_srcDestBrush(Qt::blue), m_pathBrush(QColor(96, 96, 255)), m_searchBrush(QColor(192, 255, 192)),
      m_viewportDirty(false),
      m_lButtonPressed(false),
      m_useSearchBrush(false)
    {
        setFixedSize(MAP_SIZE + 4, MAP_SIZE + 4);
        m_timerViewportDirtyCheck = startTimer(30);

        m_srcDestI[0] = m_srcDestI[1] = -1;
        m_srcDestJ[0] = m_srcDestJ[1] = -1;
    }

    virtual void paintEvent(QPaintEvent *)
    {
        QPainter p(this);

        p.drawRect(QRect(1, 1, MAP_SIZE + 1, MAP_SIZE + 1));
        
        for (int i = 1;i < GRID_W_CNT; ++i)
        {
            p.drawLine(1, 1 + i * GRID_SIZE, 1 + MAP_SIZE, 1 + i * GRID_SIZE);
        }

        for (int i = 1; i < GRID_W_CNT; ++i)
        {
            p.drawLine(1 + i * GRID_SIZE, 1, 1 + i * GRID_SIZE, 1 + MAP_SIZE);
        }

        for (int i = 0; i < GRID_W_CNT; ++i)
        {
            for (int j = 0; j < GRID_W_CNT; ++j)
            {
                switch (g_grid[i][j])
                {
                case GT_Barrier:
                    p.setBrush(m_barrierBrush);
                    break;
                case GT_SrcDest:
                    p.setBrush(m_srcDestBrush);
                    break;
                case GT_Path:
                    p.setBrush(m_pathBrush);
                    break;
                case GT_Searched:
                    if (!m_useSearchBrush) continue;
                    p.setBrush(m_searchBrush);
                    break;
                default:
                    continue;
                    break;
                }

                p.drawRect(1 + j * GRID_SIZE + 2, 1 + i * GRID_SIZE + 2, GRID_SIZE - 4, GRID_SIZE - 4);
            }
        }

        m_viewportDirty = false;
    }

    virtual void keyReleaseEvent(QKeyEvent *e)
    {
        if (e->key() == Qt::Key_Space)
        {
            memset(g_grid, 0, sizeof(g_grid));
            m_srcDestI[0] = m_srcDestI[1] = -1;
            m_srcDestJ[0] = m_srcDestJ[1] = -1;
            makeViewportDirty();
        }
        if (e->key() == Qt::Key_Escape)
        {
            close();
        }
        if (e->key() == Qt::Key_A)
        {
            if (m_srcDestI[0] == -1 || m_srcDestI[1] == -1 ||
                m_srcDestJ[0] == -1 || m_srcDestJ[1] == -1)
                return;

            doAstarWayFinding(m_srcDestI[0], m_srcDestJ[0], m_srcDestI[1], m_srcDestJ[1]);
            makeViewportDirty();
        }
        if (e->key() == Qt::Key_C)
        {
            m_useSearchBrush = !m_useSearchBrush;   
            makeViewportDirty();
        }
        if (e->key() == Qt::Key_F1)
        {
            QMessageBox::about(this, 
                tr("关于"), 
                tr(
                "空格   : 清空地图\n"
                "A      : A*寻路\n"
                "C      : 清除A*寻路过程的现实\n"
                "ESC    : 退出程序\n"));
        }
    }

    virtual void mousePressEvent(QMouseEvent *ev)
    {
        if (ev->button() == Qt::LeftButton)
        {
            m_lButtonPressed = true;
            markupBarrier(ev);
            makeViewportDirty();
        }
    }

    virtual void mouseReleaseEvent(QMouseEvent *ev)
    {   
        if (ev->button() == Qt::LeftButton)
        {
            m_lButtonPressed = false;
            markupBarrier(ev);
            makeViewportDirty();
        }
        else if (ev->button() == Qt::RightButton)
        {
            markupSrcDest(ev);
            makeViewportDirty();
        }
    }

    virtual void mouseMoveEvent(QMouseEvent *ev)
    {
        if (m_lButtonPressed)
        {
            markupBarrier(ev);
            makeViewportDirty();
        }
    }

    virtual void timerEvent(QTimerEvent *e)
    {
        if (e->timerId() == m_timerViewportDirtyCheck)
        {
            if (m_viewportDirty) update();
        }
    }

private:
    bool getIJFromMouseEvent(QMouseEvent *ev, int &i, int &j)
    {
        int x = ev->pos().x();
        int y = ev->pos().y();
        if (x <= 1 || x >= MAP_SIZE + 1) return false;
        if (y <= 1 || y >= MAP_SIZE + 1) return false;

        j = (x - 1) / GRID_SIZE;
        i = (y - 1) / GRID_SIZE;
        return true;
    }

    void markupBarrier(QMouseEvent *ev)
    {
        int i, j;
        if (!getIJFromMouseEvent(ev, i, j)) return;

        if (g_grid[i][j] != GT_SrcDest) g_grid[i][j] = GT_Barrier;
    }

    void markupSrcDest(QMouseEvent *ev)
    {
        int i, j;
        if (!getIJFromMouseEvent(ev, i, j)) return;

        // 取消标记
        if (m_srcDestI[1] == i && m_srcDestJ[1] == j)
        {
            m_srcDestI[1] = m_srcDestJ[1] = -1;
            if (g_grid[i][j] == GT_SrcDest) g_grid[i][j] = 0;
            return;
        }

        if (m_srcDestI[0] == i && m_srcDestJ[0] == j)
        {
            m_srcDestI[0] = m_srcDestI[1];
            m_srcDestJ[0] = m_srcDestJ[1];
            m_srcDestI[1] = m_srcDestJ[1] = -1;
            if (g_grid[i][j] == GT_SrcDest) g_grid[i][j] = 0;
            return;
        }

        if (m_srcDestI[0] == -1 && m_srcDestJ[0] == -1)
        {
            m_srcDestI[0] = i, m_srcDestJ[0] = j;
        }
        else
        {
            if (m_srcDestI[1] != -1 && m_srcDestJ[1] != -1)
            {
                if (g_grid[m_srcDestI[1]][m_srcDestJ[1]] == GT_SrcDest)
                {
                    g_grid[m_srcDestI[1]][m_srcDestJ[1]] = 0;
                }
            }

            m_srcDestI[1] = i, m_srcDestJ[1] = j;
        }

        g_grid[i][j] = GT_SrcDest;
    }

    void makeViewportDirty()
    {
        m_viewportDirty = true;
    }

private:
    QBrush      m_barrierBrush;
    QBrush      m_srcDestBrush;
    QBrush      m_pathBrush;
    QBrush      m_searchBrush;

    bool        m_lButtonPressed;

    bool        m_viewportDirty;
    int         m_timerViewportDirtyCheck;

    bool        m_useSearchBrush;

    int         m_srcDestI[2], m_srcDestJ[2];
};

int main(int argc, char* argv[])
{
    QApplication app(argc, argv);

    QTextCodec::setCodecForTr(QTextCodec::codecForLocale());
    
    MyLabel l;
    l.show();

    return app.exec();
}