#pragma once

#include <map>

#include "IGobangAlgo.h"

class Level1ExAlgo:
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

    typedef std::vector<char>   WinSetsUse;

private:
    bool place(int x, int y, int side);
    void unPlace(int x, int y, int side);
    int tryThisGo(int &x, int &y, int minScore, int maxScore, int level);
    int tryPeerGo(int &x, int &y, int minScore, int maxScore, int level);
    void initStateScores();
    int getScore(char state);
    void getMaxScorePoints(std::vector<Point>& pts, int maxCnt);
    
private:
    std::vector<char>   m_boardData;
    bool                m_isFirst;
    int                 m_sz;
    WinSetsUse          m_winSetUse[2];
    std::map<int, int>  m_deadWinSet[2];

    bool                m_hasWin[2];

    int                 m_stateScore[32];
};