#pragma once

#include <map>

#include "IGobangAlgo.h"

class Level0ExAlgo:
    public IGobangAlgo
{
public:
    virtual void init(int sz);
    virtual void reset();
    virtual void peerGo(int x, int y);
    virtual bool thisGo(int &x, int &y);
    virtual bool canPeerWin() const;
    virtual bool canWin() const;

private:
    struct Point
    {
        int x, y;
        int score;

        bool operator < (const Point& o) const
        {
            return score < o.score;
        }

        bool operator > (const Point& o) const
        {
            return o < *this;
        }
    };

private:
    bool place(int x, int y, int side);
    void unPlace(int x, int y, int side);
    void getMaxScorePoints(std::vector<Point>& pts, int maxCnt);
    int tryThisGo(int &x, int &y, int minScore, int maxScore, int level);
    int tryPeerGo(int &x, int &y, int minScore, int maxScore, int level);

private:
    int                 m_sz;
    bool                m_firstStep;
    std::vector<char>   m_boardData;
    std::map<int, int>  m_deadWinSet[2];
    std::vector<char>   m_winSetReach[2];
    bool                m_hasWin[2];
};
