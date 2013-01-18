#include "StdAfx.h"

#include <cassert>
#include <ctime>

#include <queue>
#include <algorithm>
#include <functional>

#include "Level0.h"

WinSets_Level0* WinSets_Level0::getSingletonPtr()
{
    static WinSets_Level0 ls_ins;
    return &ls_ins;
}

const WinSets_Level0::NxNWinSet& WinSets_Level0::getNxNWinSet(int n)
{
    assert(int(m_winSetList.size()) > n);
    return m_winSetList[n].st;
}

int WinSets_Level0::getWinCount(int n) const
{
    assert(int(m_winSetList.size()) > n);
    return m_winSetList[n].cnt;
}

WinSets_Level0::WinSets_Level0()
{
    initNxNWinSetList(16);
}

void WinSets_Level0::initNxNWinSetList(int maxN)
{
    m_winSetList.resize(6);

    for (int i = 6; i <= maxN; ++i)
    {
        enumNxNWinSets(i);
    }
}

void WinSets_Level0::onBeginEnumeration(int winSetCnt)
{
    m_winSetList.push_back(NxNWinItem());
    NxNWinItem& item = m_winSetList.back();

    size_t n = m_winSetList.size() - 1;
    item.st.resize(n);
    for (size_t i = 0; i < n; ++i)
    {
        item.st[i].resize(n);
    }

    item.cnt = winSetCnt;
}

void WinSets_Level0::onWinSet(int winSetID, int x, int y)
{
    m_winSetList.back().st[x][y].push_back(winSetID);
}

void WinSets_Level0::onEndEnumeration()
{

}



Level0Algo::Level0Algo(void)
{
}

Level0Algo::~Level0Algo(void)
{
}

void Level0Algo::init(int sz)
{
    m_boardData.resize(sz * sz, 0);
    
    int winCnt = WinSets_Level0::getSingletonPtr()->getWinCount(sz);
    m_winSetReach.resize(winCnt, 0);
    m_peerWinSetReach.resize(winCnt, 0);
    m_sz = sz;

    m_firstStep = true;
}

void Level0Algo::reset()
{
    m_boardData.assign(m_boardData.size(), 0);

    m_winSetReach.assign(m_winSetReach.size(), 0);
    m_peerWinSetReach.assign(m_peerWinSetReach.size(), 0);

    m_deadWinSet.clear();
    m_peerDeadWinSet.clear();

    m_firstStep = true;
}

void Level0Algo::peerGo(int x, int y)
{
    m_firstStep = false;

    m_boardData[y * m_sz + x] = 1;
    
    const IntVec& winIDs = WinSets_Level0::getSingletonPtr()->getNxNWinSet(m_sz)[y][x];

    for (IntVec::const_iterator iter = winIDs.begin();
        iter != winIDs.end();
        ++iter)
    {
        int winID = *iter;

        m_deadWinSet.insert(winID);

        if (m_peerDeadWinSet.count(winID) == 0)
        {
            ++m_peerWinSetReach[winID];
        }
    }
}

int Level0Algo::calcScore(std::vector<char>& s)
{
    std::sort(s.begin(), s.end(), std::greater<int>());

    int score = 0;

    while (!s.empty() && s.back() < 3)
    {
        score += s.back();
        s.pop_back();
    }

    if (s.empty())
    {
        return score;
    }

    if (s[0] == 5)
    {
        score += 10000;
    }
    else
    {
        int tScore = 1;
        for (std::vector<char>::iterator iter = s.begin();
            iter != s.end();
            ++iter)
        {
            tScore *= *iter;
        }

        score += tScore;
    }

    return score;
}

void Level0Algo::getMaxScorePoss(std::vector<int> &xs, std::vector<int> &ys, int maxCnt)
{
    struct ChessPos
    {
        int x, y;
        int score;

        bool operator < (const ChessPos& o) const
        {
            return score < o.score;
        }   

        bool operator > (const ChessPos& o) const
        {
            return o < *this;
        }
    };

    std::priority_queue<ChessPos, std::vector<ChessPos>, std::greater<ChessPos>> q;

    for (int y = 0; y < m_sz; ++y)
    {
        for (int x = 0; x < m_sz; ++x)
        {
            if (m_boardData[y * m_sz + x] != 0)
            {
                continue;
            }

            const IntVec& winIDs = WinSets_Level0::getSingletonPtr()->getNxNWinSet(m_sz)[y][x];

            std::vector<char> vThis;
            std::vector<char> vPeer;

            for (IntVec::const_iterator iter = winIDs.begin();
                iter != winIDs.end();
                ++iter)
            {
                int winID = *iter;

                if (m_deadWinSet.count(winID) == 0 && m_winSetReach[winID] != 0)
                {
                    vThis.push_back(m_winSetReach[winID] + 1);
                }

                if (m_peerDeadWinSet.count(winID) == 0 && m_peerWinSetReach[winID] != 0)
                {
                    vPeer.push_back(m_peerWinSetReach[winID] + 1);
                }
            }

            int _thisMax = calcScore(vThis);
            int _peerMax = calcScore(vPeer);

            ChessPos pos;
            pos.x = x;
            pos.y = y;
            pos.score = _thisMax + _peerMax;
            q.push(pos);
            if (int(q.size()) > maxCnt)
            {
                q.pop();
            }
        }
    }

    while (!q.empty())
    {
        ChessPos pos = q.top();
        xs.push_back(pos.x);
        ys.push_back(pos.y);
        q.pop();
    }
}

bool Level0Algo::thisGo(int &_x, int &_y)
{
    if (m_firstStep)
    {
        m_firstStep = false;
    
        _x = rand() % m_sz;
        _y = rand() % m_sz;
    }
    else
    {
        size_t winSetCnt = WinSets_Level0::getSingletonPtr()->getWinCount(m_sz);
        if (m_deadWinSet.size() == winSetCnt &&
            m_peerDeadWinSet.size() == winSetCnt)
        {
            return false;
        }

        std::vector<int> xs, ys;
        getMaxScorePoss(xs, ys, 1);

        _x = xs.front();
        _y = ys.front();
    }

    m_boardData[_y * m_sz + _x] = 2;
    const IntVec& winIDs = WinSets_Level0::getSingletonPtr()->getNxNWinSet(m_sz)[_y][_x];

    for (IntVec::const_iterator iter = winIDs.begin();
        iter != winIDs.end();
        ++iter)
    {
        int winID = *iter;

        m_peerDeadWinSet.insert(winID);

        if (m_deadWinSet.count(winID) == 0)
        {
            ++m_winSetReach[winID];
        }
    }

    return true;
}

bool Level0Algo::canPeerWin() const
{
    for (std::vector<char>::const_iterator iter = m_peerWinSetReach.begin();
        iter != m_peerWinSetReach.end();
        ++iter)
    {
        if (*iter >= 5)
        {
            return true;
        }
    }
    return false;
}

bool Level0Algo::canWin() const
{
    for (std::vector<char>::const_iterator iter = m_winSetReach.begin();
        iter != m_winSetReach.end();
        ++iter)
    {
        if (*iter >= 5)
        {
            return true;
        }
    }
    return false;
}

static IGobangAlgo* createLevel0()
{
    return new Level0Algo;
}

void registerLevel0Creator()
{
    getAlgoMap()["Level0"] = createLevel0;
}