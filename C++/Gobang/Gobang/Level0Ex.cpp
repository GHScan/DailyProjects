#include "StdAfx.h"

#include <cassert>

#include <limits>
#include <queue>
#include <exception>

#include "Level0.h"
#include "Level0Ex.h"

#undef min
#undef max

void Level0ExAlgo::init(int sz)
{
    m_sz = sz;

    m_boardData.resize(m_sz * m_sz, 0);

    int winSetCnt = WinSets_Level0::getSingletonPtr()->getWinCount(m_sz);
    m_winSetReach[0].resize(winSetCnt, 0);
    m_winSetReach[1].resize(winSetCnt, 0);
}

void Level0ExAlgo::reset()
{
    m_firstStep = true;

    m_boardData.assign(m_boardData.size(), 0);
    m_winSetReach[0].assign(m_winSetReach[0].size(), 0);
    m_winSetReach[1].assign(m_winSetReach[1].size(), 0);
    m_deadWinSet[0].clear(); 
    m_deadWinSet[1].clear();
    m_hasWin[0] = false;
    m_hasWin[1] = false;
}

bool Level0ExAlgo::place(int x, int y, int side)
{
    bool win = false;

    assert(m_boardData[y * m_sz + x] == 0);

    m_boardData[y * m_sz + x] = side + 1;

    const IntVec& winSets = WinSets_Level0::getSingletonPtr()->getNxNWinSet(m_sz)[y][x];

    for (IntVec::const_iterator iter = winSets.begin();
        iter != winSets.end();
        ++iter)
    {
        int winSetID = *iter;

        win = (++m_winSetReach[side][winSetID] >= 5) || win;
        ++m_deadWinSet[1 - side][winSetID];
    }

    return win;
}

void Level0ExAlgo::unPlace(int x, int y, int side)
{
    assert(m_boardData[y * m_sz + x] == side + 1);

    m_boardData[y * m_sz + x] = 0;

    const IntVec& winSets = WinSets_Level0::getSingletonPtr()->getNxNWinSet(m_sz)[y][x];

    for (IntVec::const_iterator iter = winSets.begin();
        iter != winSets.end();
        ++iter)
    {
        int winSetID = *iter;

        --m_winSetReach[side][winSetID];

        std::map<int, int>::iterator _iter = m_deadWinSet[1 - side].find(winSetID);
        --_iter->second;
        if (_iter->second == 0)
        {
            m_deadWinSet[1- side].erase(_iter);
        }
    }
}

void Level0ExAlgo::peerGo(int x, int y)
{
    m_firstStep = false;

    m_hasWin[1] = place(x, y, 1);
}

void Level0ExAlgo::getMaxScorePoints(std::vector<Point>& pts, int maxCnt)
{
    std::priority_queue<Point, std::vector<Point>, std::greater<Point>> q;

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

                if (m_deadWinSet[0].count(winID) == 0 && m_winSetReach[0][winID] != 0)
                {
                    vThis.push_back(m_winSetReach[0][winID] + 1);
                }

                if (m_deadWinSet[1].count(winID) == 0 && m_winSetReach[1][winID] != 0)
                {
                    vPeer.push_back(m_winSetReach[1][winID] + 1);
                }
            }

            int _thisMax = Level0Algo::calcScore(vThis);
            int _peerMax = Level0Algo::calcScore(vPeer);

            Point pos;
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

    pts.resize(q.size());
    size_t i = pts.size() - 1;
    while (!q.empty())
    {
        pts[i--] = q.top();
        q.pop();
    }
}

int Level0ExAlgo::tryThisGo(int &x, int &y, int minScore, int maxScore, int level)
{
    if (minScore > maxScore)
    {
        return maxScore;
    }

    bool ignore = true;
    int score = minScore;

    std::vector<Point> pts;
    getMaxScorePoints(pts, 5);

    if (!pts.empty())
    {
        x = pts.front().x;
        y = pts.front().y;
    }

    for (std::vector<Point>::const_iterator iter = pts.begin();
        iter != pts.end();
        ++iter)
    {
        if (minScore > maxScore)
        {
            break;
        }

        ignore = false;

        const Point& pt = *iter;
        int _score = pt.score;
        if (level > 0)
        {
            if (place(pt.x, pt.y, 0))
            {
                _score = std::numeric_limits<int>::max() / 2;
            }
            else
            {
                int _x, _y;
                _score += tryPeerGo(_x, _y, minScore - _score, maxScore, level - 1);
            }
            unPlace(pt.x, pt.y, 0);
        }
        
        if (score < _score || 
            (score == _score && rand() % 2 == 0))
        {
            x = pt.x, y = pt.y;
            score = _score;
        }

        minScore = std::max(score, minScore);
    }

    if (ignore)
    {
        return maxScore;
    }
    return score;
}

int Level0ExAlgo::tryPeerGo(int &x, int &y, int minScore, int maxScore, int level)
{
    if (minScore > maxScore)
    {
        return minScore;
    }

    bool ignore = true;
    int score = maxScore;

    std::vector<Point> pts;
    getMaxScorePoints(pts, 5);

    for (std::vector<Point>::const_iterator iter = pts.begin();
        iter != pts.end();
        ++iter)
    {
        if (minScore > maxScore)
        {
            break;
        }

        ignore = false;

        const Point& pt = *iter;
        int _score = -pt.score;
        if (place(pt.x, pt.y, 1))
        {
            _score = std::numeric_limits<int>::min() / 2;
        }
        else
        {
            int _x = 0, _y = 0;
            _score += tryThisGo(_x, _y, minScore, maxScore + _score, level);
        }   
        unPlace(pt.x, pt.y, 1);

        if (score > _score || 
            (score == _score && rand() % 2 == 0))
        {
            x = pt.x, y = pt.y;
            score = _score;
        }

        maxScore = std::min(score, maxScore);
    }

    if (ignore)
    {
        return minScore;
    }
    return score;
}

bool Level0ExAlgo::thisGo(int &x, int &y)
{
    if (m_firstStep)
    {
        m_firstStep = false;

        x = rand() % m_sz;
        y = rand() % m_sz;
    }
    else
    {
        int winSetCnt = WinSets_Level0::getSingletonPtr()->getWinCount(m_sz);
        if (m_deadWinSet[0].size() == winSetCnt &&
            m_deadWinSet[1].size() == winSetCnt)
        {
            return false;
        }

        tryThisGo(x, y, std::numeric_limits<int>::min() / 4, std::numeric_limits<int>::max() / 4, 2);
    }

    m_hasWin[0] = place(x, y, 0);

    return true;
}

bool Level0ExAlgo::canPeerWin() const
{
    return m_hasWin[1];
}

bool Level0ExAlgo::canWin() const
{
    return m_hasWin[0];
}

static IGobangAlgo* createLevel0Ex()
{
    return new Level0ExAlgo;
}

void registerLevel0ExCreator()
{
    getAlgoMap()["Level0Ex"] = createLevel0Ex;
}