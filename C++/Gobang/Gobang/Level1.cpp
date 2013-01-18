#include "StdAfx.h"

#include <cassert>

#include <vector>

#include "Level1.h"

WinSets_Level1* WinSets_Level1::getSingletonPtr()
{
    static WinSets_Level1 ls_ins;
    return &ls_ins;
}   

WinSets_Level1::WinSets_Level1()
{
    initAllNxN(16);
}

void WinSets_Level1::initAllNxN(int maxN)
{
    m_nxnList.resize(6);
    m_nxnAllList.resize(6);

    for (int i = 6; i <= maxN; ++i)
    {
        enumNxNWinSets(i);
    }
}

void WinSets_Level1::setEnvNxN(int n)
{
    m_nxn = &m_nxnList[n];
    m_nxnAll = &m_nxnAllList[n];
}

const IntVec& WinSets_Level1::getWinSets(int x, int y) const
{
    return (*m_nxn)[y][x];
}

const WinSets_Level1::WinSet& WinSets_Level1::getWinSetPoss(int winSetID) const
{
    return (*m_nxnAll)[winSetID];
}

int WinSets_Level1::getWinSetCount() const
{
    return int(m_nxnAll->size());
}

void WinSets_Level1::onBeginEnumeration(int winSetCnt)
{
    size_t n = m_nxnList.size();

    m_nxnList.push_back(NxNWinSets());
    m_nxn = &m_nxnList.back();
    m_nxn->resize(n);
    for (size_t i = 0; i < n; ++i)
    {
        (*m_nxn)[i].resize(n);
    }

    m_nxnAllList.push_back(NxNWinSetValues());
    m_nxnAll = &m_nxnAllList.back();
    m_nxnAll->resize(winSetCnt);
    for (int i = 0; i < winSetCnt; ++i)
    {
        (*m_nxnAll)[i].pt0[0] = -1;
    }
}

void WinSets_Level1::onWinSet(int winSetID, int x, int y)
{
    (*m_nxn)[y][x].push_back(winSetID);

    WinSet &st = (*m_nxnAll)[winSetID];

    if (st.pt0[0] == -1)
    {
        st.pt0[0] = x, st.pt0[1] = y;
    }
    else
    {
        st.pt1[0] = x, st.pt1[1] = y;
    }
}

void WinSets_Level1::onEndEnumeration()
{

}


Level1Algo::Level1Algo(void)
{
    initDefaultStateScore();
}

Level1Algo::~Level1Algo(void)
{
}

void Level1Algo::init(int sz)
{
    m_sz = sz;

    m_boardData.resize(sz * sz, 0);

    WinSets_Level1::getSingletonPtr()->setEnvNxN(sz);
    int cnt = WinSets_Level1::getSingletonPtr()->getWinSetCount();
    m_thisUse.resize(cnt, Use());
    m_peerUse.resize(cnt, Use());
}

void Level1Algo::reset()
{
    m_boardData.assign(m_boardData.size(), 0);
    m_thisUse.assign(m_thisUse.size(), Use());
    m_peerUse.assign(m_peerUse.size(), Use());

    m_deadWinSet.clear();
    m_peerDeadWinSet.clear();

    m_isFirst = true;
}

static inline int getIndexInWinSet(int winSetID, int x, int y)
{
    const WinSets_Level1::WinSet& winSet = WinSets_Level1::getSingletonPtr()->getWinSetPoss(winSetID);
    int id = 0;
    if (winSet.pt0[0] != winSet.pt1[0])
    {
        id = (x - winSet.pt0[0]) * 4 / (winSet.pt1[0] - winSet.pt0[0]);
    }
    else
    {
        id = (y - winSet.pt0[1]) * 4 / (winSet.pt1[1] - winSet.pt0[1]);
    }
    return id;
}

void Level1Algo::peerGo(int x, int y)
{
    m_boardData[y * m_sz + x] = 1;

    const IntVec &winSets = WinSets_Level1::getSingletonPtr()->getWinSets(x, y);
    for (IntVec::const_iterator iter = winSets.begin();
        iter != winSets.end();
        ++iter)
    {
        int winSetID = *iter;

        m_thisUse[winSetID].cnt = -1;

        if (m_peerUse[winSetID].cnt != -1)
        {
            ++m_peerUse[winSetID].cnt;
            m_peerUse[winSetID].state |= 1 << getIndexInWinSet(winSetID, x, y);
        }

        m_deadWinSet.insert(winSetID);
    }

    m_isFirst = false;
}

int getBit1Count(int bits)
{
    int cnt = 0;
    while (bits != 0)
    {
        bits &= bits - 1;
        ++cnt;
    }
    return cnt;
}

void Level1Algo::initDefaultStateScore()
{
    memset(m_stateScores, 0, sizeof(m_stateScores));

    // 5
    m_stateScores[0x1f] = 1000;

    // 4
    m_stateScores[0x0f] = 
    m_stateScores[0x1e] = 97;

    m_stateScores[0x17] =
    m_stateScores[0x1b] =
    m_stateScores[0x1d] = 48;

    // 3
    m_stateScores[0x0e] = 47;

    m_stateScores[0x1c] = 
    m_stateScores[0x0e] = 
    m_stateScores[0x07] = 23;

    m_stateScores[0x1a] = 
    m_stateScores[0x19] = 
    m_stateScores[0x16] = 
    m_stateScores[0x15] = 
    m_stateScores[0x13] = 
    m_stateScores[0x0b] = 
    m_stateScores[0x0d] = 20;

    // 2
    m_stateScores[0x18] = 
    m_stateScores[0x0c] = 
    m_stateScores[0x06] = 
    m_stateScores[0x03] = 4;

    m_stateScores[0x14] = 
    m_stateScores[0x12] = 
    m_stateScores[0x11] = 
    m_stateScores[0x0a] = 
    m_stateScores[0x09] = 
    m_stateScores[0x05] = 3;
}

void Level1Algo::setStateScore(char state, int score)
{
    m_stateScores[state] = score;
}

int Level1Algo::getStateScore(char state)
{
    return m_stateScores[state];
}

bool Level1Algo::thisGo(int &_x, int &_y)
{
    if (m_isFirst)
    {
        m_isFirst = false;

        _x = rand() % m_sz;
        _y = rand() % m_sz;
    }
    else
    {
        size_t winSetCnt = WinSets_Level1::getSingletonPtr()->getWinSetCount();
        if (m_deadWinSet.size() == winSetCnt &&
            m_peerDeadWinSet.size() == winSetCnt)
        {
            return false;
        }

        int thisScore = 0;
        int thisX = 0, thisY = 0;
        int peerScore = 0;
        int peerX = 0, peerY = 0;

        for (int y = 0; y < m_sz; ++y)
        {
            for (int x = 0; x < m_sz; ++x)
            {
                if (m_boardData[y * m_sz + x] != 0)
                {
                    continue;
                }

                int _thisScore = 0;
                int _peerScore = 0;

                const IntVec &winSets = WinSets_Level1::getSingletonPtr()->getWinSets(x, y);
                for (IntVec::const_iterator iter = winSets.begin();
                    iter != winSets.end();
                    ++iter)
                {
                    int winSetID = *iter;

                    int stateChange = 1 << getIndexInWinSet(winSetID, x, y);

                    const Use& thisUs = m_thisUse[winSetID];
                    if (thisUs.cnt != -1)
                    {
                        _thisScore += getStateScore(thisUs.state | stateChange);
                    }

                    const Use& peerUs = m_peerUse[winSetID];
                    if (peerUs.cnt != -1)
                    {
                        _peerScore += getStateScore(peerUs.state | stateChange);
                    }
                }

                if (_thisScore > thisScore || (_thisScore == thisScore && rand() % 2 == 0))
                {
                    thisScore = _thisScore;
                    thisX = x, thisY = y;
                }
                if (_peerScore > peerScore || (_peerScore == peerScore && rand() % 2 == 0))
                {
                    peerScore = _peerScore;
                    peerX = x, peerY = y;
                }
            }
        }

        if (thisScore > peerScore)
        {
            _x = thisX, _y = thisY;
        }
        else
        {
            _x = peerX, _y = peerY;
        }
    }

    m_boardData[_y * m_sz + _x] = 2;

    const IntVec &winSets = WinSets_Level1::getSingletonPtr()->getWinSets(_x, _y);
    for (IntVec::const_iterator iter = winSets.begin();
        iter != winSets.end();
        ++iter)
    {
        int winSetID = *iter;

        m_peerUse[winSetID].cnt = -1;

        if (m_thisUse[winSetID].cnt != -1)
        {
            ++m_thisUse[winSetID].cnt;
            m_thisUse[winSetID].state |= 1 << getIndexInWinSet(winSetID, _x, _y);
        }

        m_peerDeadWinSet.insert(winSetID);
    }

    return true;
}

bool Level1Algo::canPeerWin() const
{
    for (WinSetsUse::const_iterator iter = m_peerUse.begin();
        iter != m_peerUse.end();
        ++iter)
    {
        if (iter->cnt >= 5)
        {
            return true;
        }
    }

    return false;
}

bool Level1Algo::canWin() const
{   
    for (WinSetsUse::const_iterator iter = m_thisUse.begin();
        iter != m_thisUse.end();
        ++iter)
    {
        if (iter->cnt >= 5)
        {
            return true;
        }
    }

    return false;
}

static IGobangAlgo* createLevel1()
{
    return new Level1Algo;
}

void registerLevel1Creator()
{
    getAlgoMap()["Level1"] = createLevel1;
}