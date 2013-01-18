#pragma once

#include <vector>
#include <set>

#include "IGobangAlgo.h"
#include "WinSetsTemplate.h"

typedef std::vector<int>    IntVec;

class WinSets_Level0:
    public WinSetsTemplate
{
public:
    typedef std::vector<std::vector<IntVec>>    NxNWinSet;

public:
    static WinSets_Level0* getSingletonPtr();

    const NxNWinSet& getNxNWinSet(int n);
    int getWinCount(int n) const;

private:
    WinSets_Level0();
    void initNxNWinSetList(int maxN);

    virtual void onBeginEnumeration(int winSetCnt);
    virtual void onWinSet(int winSetID, int x, int y);
    virtual void onEndEnumeration();

private:
    struct NxNWinItem
    {
        NxNWinSet st;
        int       cnt;
    };

    typedef std::vector<NxNWinItem>  NxNWinSetList;

private:
    NxNWinSetList   m_winSetList;
};

class Level0Algo:
    public IGobangAlgo
{
public:
    Level0Algo(void);
    ~Level0Algo(void);

    virtual void init(int sz);
    virtual void reset();
    virtual void peerGo(int x, int y);
    virtual bool thisGo(int &x, int &y);
    virtual bool canPeerWin() const;
    virtual bool canWin() const;

    static int calcScore(std::vector<char>& s);

protected:
    void getMaxScorePoss(std::vector<int> &xs, std::vector<int> &ys, int maxCnt);

protected:
    int                 m_sz;
    std::vector<char>   m_boardData;
    std::set<int>       m_deadWinSet;
    std::vector<char>   m_winSetReach;
    std::set<int>       m_peerDeadWinSet;
    std::vector<char>   m_peerWinSetReach;
    bool                m_firstStep;
};
