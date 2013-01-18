// Console.cpp : 定义控制台应用程序的入口点。
//

#include "stdafx.h"

#include <boost/progress.hpp>

#include <cmath>
#include <cassert>

#include "AstarAlgo.h"

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
    Node   *parent;
    uint    pos;
    ushort  cost;
    ushort  distance;
    size_t  userData;

    void setPos(int i, int j)
    {
        pos = (i << 16) | j;
    }

    void getPos(int &i, int &j)
    {
        i = pos >> 16;
        j = pos & 0xffff;
    }

    void calcDistance(int destI, int destJ)
    {
        distance = (abs(destI - (int)(pos >> 16)) + abs(destJ - (int)(pos & 0xffff))) * 2;
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
};

class AstarMapProviderImpl:
    private IAstarMapProvider
{
public:
    AstarMapProviderImpl():
    m_src(NULL), m_dest(NULL)
    {
        for (int i = 0; i < GRID_W_CNT; ++i)
        {
            for (int j = 0; j < GRID_W_CNT; ++j)
            {
                m_nodes[i][j].setPos(i, j);
            }
        }
    }

    virtual NodeT* getSrcNode() { return (NodeT*)m_src; }
    virtual NodeT* getDestNode() { return (NodeT*)m_dest; }

    virtual void setParent(NodeT* child, NodeT* parent) { ((Node*)child)->parent = (Node*)parent; }
    virtual NodeT* getParent(NodeT* node) { return (NodeT*)((Node*)node)->parent; }

    virtual void resetScore(NodeT* _node, bool cost = true, bool distance = true)
    {
        Node* node = (Node*)_node;
        if (cost) node->calcCost();
        if (distance) 
        {
            int i, j;
            m_dest->getPos(i, j);
            node->calcDistance(i, j);
        }
    }

    virtual bool isPathNearer(NodeT* _testNode, NodeT *_newParent)
    {
        Node *testNode = (Node*)_testNode;
        Node *newParent = (Node*)_newParent;        
        assert(testNode->cost > 0);
        ushort oldCost = testNode->cost;
        testNode->parent = newParent;
        testNode->calcCost();
        ushort newCost = testNode->cost;
        testNode->cost = oldCost;
        return newCost < oldCost;
    }

    virtual void maskAsPath(NodeT* node)
    {
        int i, j;
        ((Node*)node)->getPos(i, j);
        assert(g_grid[i][j] == 0);
        g_grid[i][j] = GT_Path;
    }

    virtual void maskAsFound(NodeT* node)
    {
        int i, j;
        ((Node*)node)->getPos(i, j);
        if (g_grid[i][j] == 0) g_grid[i][j] = GT_Searched;
    }

    virtual std::vector<NodeT*>& getAdjacencyNodes(NodeT* _node)
    {
        m_adjacency.clear();

        Node *node = (Node*)_node;
        int i, j;
        node->getPos(i, j);
        for (int _i = i - 1; _i <= i + 1; ++_i)
        {
            for (int _j = j - 1; _j <= j + 1; ++_j)
            {
                if (_i < 0 || _i >= GRID_W_CNT || _j < 0 || _j >= GRID_W_CNT) continue;
                if (i == _i && j == _j) continue;
                if (i != _i && j != _j) continue;

                if (g_grid[_i][_j] == GT_Barrier) continue;

                m_adjacency.push_back((NodeT*)&m_nodes[_i][_j]);
            }
        }

        return m_adjacency;
    }

    // 适配接口
    virtual bool scoreLess(NodeT* _lhs, NodeT* _rhs)
    {
        Node *lhs = (Node*)_lhs;
        Node *rhs = (Node*)_rhs;
        return lhs->cost + lhs->distance < rhs->cost + rhs->distance;
    }

    virtual void setUserData(NodeT* node, size_t data)
    {
        ((Node*)node)->userData = data;
    }

    virtual size_t getUserData(NodeT* node)
    {
        return ((Node*)node)->userData;
    }

    virtual NodeT* getMinimumScoreNode()
    {
        static Node ls_node = {0};
        return (NodeT*)&ls_node;
    }

    void excute(int srcI, int srcJ, int destI, int destJ)
    {
        m_src = &m_nodes[srcI][srcJ];
        m_dest = &m_nodes[destI][destJ];

        for (int i = 0; i < GRID_W_CNT; ++i)
        {
            for (int j = 0; j < GRID_W_CNT; ++j)
            {
                if (g_grid[i][j] == GT_Searched || g_grid[i][j] == GT_Path) g_grid[i][j] = 0;
            }
        }

        doAstarAlgo(this);
    }

private:
    Node                m_nodes[GRID_W_CNT][GRID_W_CNT];
    Node*               m_src;
    Node*               m_dest;
    std::vector<NodeT*> m_adjacency;
};

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

            {
                boost::progress_timer __timer;

                AstarMapProviderImpl astar;

#ifndef _DEBUG
                for (int i = 0; i < 100; ++i)
#else
                for (int i = 0; i < 1; ++i)
#endif
                astar.excute(m_srcDestI[0], m_srcDestJ[0], m_srcDestI[1], m_srcDestJ[1]);
            }
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
                "S      : 保存当前地图\n"
                "L      : 读取新地图\n"
                "ESC    : 退出程序\n"));
        }
        if (e->key() == Qt::Key_S)
        {
            QString path = QFileDialog::getSaveFileName(
                this, 
                tr("选择保存路径"), "", tr("A*格式地图 (*.astar)"));
            if (path.isEmpty()) return;

            QFile f(path);
            if (f.open(QIODevice::WriteOnly))
            {
                QDataStream s(&f);
                for (int i = 0; i < GRID_W_CNT; ++i)
                {
                    for (int j = 0; j < GRID_W_CNT; ++j)
                    {
                        s << ((g_grid[i][j] == GT_Barrier || g_grid[i][j] == GT_SrcDest) ? g_grid[i][j] : 0);
                    }
                }
            }
        }
        if (e->key() == Qt::Key_L)
        {
            QString path = QFileDialog::getOpenFileName(
                this, 
                tr("选择要打开的文件"), "", tr("A*格式地图 (*.astar)"));
            if (path.isEmpty()) return;

            m_srcDestI[0] = m_srcDestJ[0] = m_srcDestI[1] = m_srcDestJ[1] = -1;

            QFile f(path);
            if (f.open(QIODevice::ReadOnly))
            {
                QDataStream s(&f);
                for (int i = 0; i < GRID_W_CNT; ++i)
                {
                    for (int j = 0; j < GRID_W_CNT; ++j)
                    {
                        s >> g_grid[i][j];
                        if (g_grid[i][j] == GT_SrcDest)
                        {
                            if (m_srcDestI[0] == -1) m_srcDestI[0] = i, m_srcDestJ[0] = j;
                            else m_srcDestI[1] = i, m_srcDestJ[1] = j;
                        }
                    }
                }
            }

            makeViewportDirty();
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

    // QTextCodec::setCodecForTr(QTextCodec::codecForLocale());
    QTextCodec::setCodecForTr(QTextCodec::codecForName("gb2312"));
    
    MyLabel l;
    l.show();

    return app.exec();
}