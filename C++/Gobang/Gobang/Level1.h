#pragma once

#include <vector>
#include <set>

#include "IGobangAlgo.h"

#include "WinSetsTemplate.h"

typedef std::vector<int>    IntVec;

class WinSets_Level1:
    public WinSetsTemplate
{
public:
    typedef std::vector<IntVec>         RowWinSets;
    typedef std::vector<RowWinSets>     NxNWinSets;
    typedef std::vector<NxNWinSets>     NxNWinSetsList;

    struct WinSet
    {
        int pt0[2];
        int pt1[2];
    };
    typedef std::vector<WinSet>             NxNWinSetValues;
    typedef std::vector<NxNWinSetValues>    NxNWinSetValuesList;

public:
    static WinSets_Level1* getSingletonPtr();

    void setEnvNxN(int n);
    const IntVec& getWinSets(int x, int y) const;
    const WinSet& getWinSetPoss(int winSetID) const;
    int getWinSetCount() const;

private:
    WinSets_Level1();

    void initAllNxN(int maxN);

    virtual void onBeginEnumeration(int winSetCnt);
    virtual void onWinSet(int winSetID, int x, int y);
    virtual void onEndEnumeration();

private:
    NxNWinSetsList          m_nxnList;
    NxNWinSets             *m_nxn;
    NxNWinSetValuesList     m_nxnAllList;
    NxNWinSetValues        *m_nxnAll;
};

class Level1Algo:
    public IGobangAlgo
{
public:
    Level1Algo(void);
    ~Level1Algo(void);

    void setStateScore(char state, int score);

private:
    virtual void init(int sz);
    virtual void reset();
    virtual void peerGo(int x, int y);
    virtual bool thisGo(int &x, int &y);
    virtual bool canPeerWin() const;
    virtual bool canWin() const;

private:
    int getStateScore(char state);
    void initDefaultStateScore();

private:
    struct Use
    {
        Use(): state(0), cnt(0) {}
        char state;
        char cnt;
    };
    typedef std::vector<Use>    WinSetsUse;
    
private:
    WinSetsUse          m_thisUse;
    WinSetsUse          m_peerUse;
    std::vector<char>   m_boardData;
    bool                m_isFirst;
    int                 m_sz;
    int                 m_stateScores[32];

    std::set<int>   m_deadWinSet;
    std::set<int>   m_peerDeadWinSet;
};
