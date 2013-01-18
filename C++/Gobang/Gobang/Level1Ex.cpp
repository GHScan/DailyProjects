#include "StdAfx.h"

#include <cassert>

#include <limits>
#include <queue>

#include "Level1.h"
#include "Level1Ex.h"

#undef min
#undef max

void Level1ExAlgo::init(int sz)
{
    m_sz = sz;

    m_boardData.resize(m_sz * m_sz, 0);

    // 和level1共用环境, 可能出现的bug等...WinSets_Level1::getSingletonPtr()->setEnvNxN(m_sz);
    int winSetCnt = WinSets_Level1::getSingletonPtr()->getWinSetCount();
    m_winSetUse[0].resize(winSetCnt, 0);
    m_winSetUse[1].resize(winSetCnt, 0);
    
    initStateScores();
}

void Level1ExAlgo::reset()
{
    m_boardData.assign(m_boardData.size(), 0);

    m_winSetUse[0].assign(m_winSetUse[0].size(), 0);
    m_winSetUse[1].assign(m_winSetUse[1].size(), 0);
    m_deadWinSet[0].clear();
    m_deadWinSet[1].clear();

    m_isFirst = true;
    m_hasWin[0] = false;
    m_hasWin[1] = false;
}

void Level1ExAlgo::peerGo(int x, int y)
{
    m_isFirst = false;   

    m_hasWin[1] = place(x, y, 1);
}

bool Level1ExAlgo::thisGo(int &x, int &y)
{
    if (m_isFirst)
    {
        m_isFirst = false;

        x = rand() % m_sz;
        y = rand() % m_sz;
    }
    else
    {
        int winSetCnt = WinSets_Level1::getSingletonPtr()->getWinSetCount();
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

bool Level1ExAlgo::canPeerWin() const
{
    return m_hasWin[1];
}

bool Level1ExAlgo::canWin() const
{
    return m_hasWin[0];
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


bool Level1ExAlgo::place(int x, int y, int side)
{
    assert(m_boardData[y * m_sz + x] == 0);

    bool win = false;

    m_boardData[y * m_sz + x] = side + 1;

    const IntVec & winSets = WinSets_Level1::getSingletonPtr()->getWinSets(x, y);
    for (IntVec::const_iterator iter = winSets.begin();
        iter != winSets.end();
        ++iter)
    {
        int winSetID = *iter;

        m_winSetUse[side][winSetID] |= 1 << getIndexInWinSet(winSetID, x, y);
        win = m_winSetUse[side][winSetID] == 31 || win;

        ++m_deadWinSet[1 - side][winSetID];
    }

    return win;
}

void Level1ExAlgo::unPlace(int x, int y, int side)
{   
    assert(m_boardData[y * m_sz + x] == side + 1);

    m_boardData[y * m_sz + x] = 0;

    const IntVec & winSets = WinSets_Level1::getSingletonPtr()->getWinSets(x, y);
    for (IntVec::const_iterator iter = winSets.begin();
        iter != winSets.end();
        ++iter)
    {
        int winSetID = *iter;

        m_winSetUse[side][winSetID] &= ~(1 << getIndexInWinSet(winSetID, x, y));
        
        std::map<int, int>::iterator _iter = m_deadWinSet[1 - side].find(winSetID);
        --_iter->second;
        if (_iter->second == 0)
        {
            m_deadWinSet[1 - side].erase(_iter);
        }
    }
}

int Level1ExAlgo::tryThisGo(int &x, int &y, int minScore, int maxScore, int level)
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

int Level1ExAlgo::tryPeerGo(int &x, int &y, int minScore, int maxScore, int level)
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

void Level1ExAlgo::getMaxScorePoints(std::vector<Point>& pts, int maxCnt)
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

            int _thisScore = 1;
            int _peerScore = 1;

            const IntVec &winSets = WinSets_Level1::getSingletonPtr()->getWinSets(x, y);
            for (IntVec::const_iterator iter = winSets.begin();
                iter != winSets.end();
                ++iter)
            {
                int winSetID = *iter;

                char stateChange = 1 << getIndexInWinSet(winSetID, x, y);

                char thisUs = m_winSetUse[0][winSetID];
                if (thisUs != 0 && m_deadWinSet[0].count(winSetID) == 0)
                {
                    _thisScore += getScore(thisUs | stateChange);
                }

                char peerUs = m_winSetUse[1][winSetID];
                if (peerUs != 0 && m_deadWinSet[1].count(winSetID) == 0)
                {
                    _peerScore += getScore(peerUs | stateChange);
                }
            }

            Point pos;
            pos.x = x;
            pos.y = y;
            pos.score = _thisScore + _peerScore;

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

void Level1ExAlgo::initStateScores()
{
    memset(m_stateScore, 0, sizeof(m_stateScore));

    // 5
    m_stateScore[0x1f] = 1000;

    // 4
    m_stateScore[0x0f] = 
    m_stateScore[0x1e] = 97;

    m_stateScore[0x17] =
    m_stateScore[0x1b] =
    m_stateScore[0x1d] = 48;

    // 3
    m_stateScore[0x0e] = 47;

    m_stateScore[0x1c] = 
    m_stateScore[0x0e] = 
    m_stateScore[0x07] = 23;

    m_stateScore[0x1a] = 
    m_stateScore[0x19] = 
    m_stateScore[0x16] = 
    m_stateScore[0x15] = 
    m_stateScore[0x13] = 
    m_stateScore[0x0b] = 
    m_stateScore[0x0d] = 20;

    // 2
    m_stateScore[0x18] = 
    m_stateScore[0x0c] = 
    m_stateScore[0x06] = 
    m_stateScore[0x03] = 4;

    m_stateScore[0x14] = 
    m_stateScore[0x12] = 
    m_stateScore[0x11] = 
    m_stateScore[0x0a] = 
    m_stateScore[0x09] = 
    m_stateScore[0x05] = 3;
}

int Level1ExAlgo::getScore(char state)
{
    return m_stateScore[state];
}

static IGobangAlgo* createLevel1Ex()
{
    return new Level1ExAlgo;
}

void registerLevel1ExCreator()
{
    getAlgoMap()["Level1Ex"] = createLevel1Ex;
}