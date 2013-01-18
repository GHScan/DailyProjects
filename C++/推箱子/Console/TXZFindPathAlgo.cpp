#include "stdafx.h"

#include <algorithm>

#include <hash_map>

#include "TXZMapDefine.h"

static void getTilesHasTileType(std::vector<int> &r, const TXZMap& m, TileType tt)
{
    verify(r.empty());
    for (size_t i = 0; i < m.data.size(); ++i)
    {
        if ((m.data[i] & tt) != 0) r.push_back((int)i);
    }
}
static void getAdjacencyTileIter(std::vector<int>& iter, const TXZMap& m, int i)
{
    verify(iter.empty());
    ASSERT_INSIDE_N(i, (int)m.data.size());
    int x = i % m.w, y = i / m.w;
    if (x - 1 >= 0) iter.push_back(i - 1);
    if (x + 1 < m.w) iter.push_back(i + 1);
    if (y - 1 >= 0) iter.push_back(i - m.w);
    if (y + 1 < m.h) iter.push_back(i + m.w);
}

static char idxChangeToDir(int oldIdx, int newIdx)
{
    verify(oldIdx != newIdx);
    if (abs(newIdx - oldIdx) == 1) return oldIdx < newIdx ? 'r' : 'l';
    else return oldIdx < newIdx ? 'd' : 'u';
}

static int idxAddDir(int idx, char dir, const TXZMap& m)
{
    int x = idx % m.w, y = idx / m.w;
    switch (dir)
    {
    case 'l': x = x - 1; break;
    case 'r': x = x + 1; break;
    case 'u': y = y - 1; break;
    case 'd': y = y + 1; break;
    default:
        verify(0);
        break;
    }
    if (x < 0 || x >= m.w || y < 0 || y >= m.h) return -1;
    return y * m.w + x;
}

inline char oppositeDir(char dir)
{
    switch (dir)
    {
    case 'l': return 'r';
    case 'r': return 'l';
    case 'u': return 'd';
    case 'd': return 'u';
    default: break;
    }
    verify(0);
    return 0;
}

// 注意这两个hash辅助函数都假设w、h相同
template<>
struct std::less<std::vector<int>>: 
    public std::binary_function<std::vector<int>, std::vector<int>, bool>
{
    bool operator()(const std::vector<int>& l, const std::vector<int>& r) const
    {	
        return std::lexicographical_compare(
            l.begin(), l.end(), r.begin(), r.end());
    }
};

template<>
inline size_t stdext::hash_value<std::vector<int>>(const std::vector<int>& m)
{	
    return (stdext::_Hash_value(m.begin(), m.end()));
}

typedef stdext::hash_map<std::vector<int>, int>  TXZMapStateHashMap;

class TXZMapPathFinder
{
public:
    TXZMapPathFinder(const TXZMap& m, int srcIdx):
      m_srcIdx(srcIdx)
      {
          ASSERT_INSIDE_N(srcIdx, (int)m.data.size());
          // dijkstra算法
          m_dis.resize(m.data.size(), MAX_DIS);
          m_dis[m_srcIdx] = 0;
          m_lastIdx.resize(m.data.size(), -1);

          static std::vector<int> searchList;
          searchList.clear();
          searchList.push_back(m_srcIdx);

          static std::vector<int> tileIter;
          tileIter.clear();
          while (!searchList.empty())
          {
              int idx = searchList.back();
              searchList.pop_back();

              tileIter.clear();
              getAdjacencyTileIter(tileIter, m, idx);
              for (size_t i = 0; i < tileIter.size(); ++i)
              {
                  int adjacencyIdx = tileIter[i];
                  if ((m.data[adjacencyIdx] & (TT_Box | TT_Wall)) != 0) continue;
                  if (m_dis[idx] + 1 < m_dis[adjacencyIdx])
                  {
                      m_dis[adjacencyIdx] = m_dis[idx] + 1;
                      m_lastIdx[adjacencyIdx] = idx;
                      searchList.push_back(adjacencyIdx);
                  }
              }
          }
      }

      bool isReachable(int destIdx) const
      {
          ASSERT_INSIDE_N(destIdx, (int)m_dis.size());
          return m_dis[destIdx] != MAX_DIS;
      }

      std::string getPath(int destIdx) const
      {
          ASSERT_INSIDE_N(destIdx, (int)m_dis.size());
          verify(destIdx != m_srcIdx);
          if (!isReachable(destIdx)) return "";

          std::string path;
          int idx = destIdx;
          while (idx != m_srcIdx)
          {
              path.push_back(idxChangeToDir(m_lastIdx[idx], idx));
              idx = m_lastIdx[idx];
          }
          std::reverse(path.begin(), path.end());
          return path;
      }

private:
    std::vector<int>    m_lastIdx;
    std::vector<int>    m_dis;
    int m_srcIdx;
    static const int MAX_DIS = 1 << 30;
};

static bool quickCheckIsTXZMapSuccessable(const TXZMap& m)
{
    static std::vector<int> boxTiles;
    boxTiles.clear();
    getTilesHasTileType(boxTiles, m, TT_Box);

    for (size_t i = 0; i < boxTiles.size(); ++i)
    {
        int boxIdx = boxTiles[i];
        if ((m.data[boxIdx] & TT_Dest) != 0) continue;

        int x = boxIdx % m.w, y = boxIdx / m.w;

        bool horPushable = 
            (x - 1 >= 0 && x + 1 < m.w) && 
            (m.data[boxIdx - 1] & (TT_Wall)) == 0 &&
            (m.data[boxIdx + 1] & (TT_Wall)) == 0;
        bool verPushable = 
            (y - 1 >= 0 && y + 1 < m.h) && 
            (m.data[boxIdx - m.w] & (TT_Wall)) == 0 && 
            (m.data[boxIdx + m.w] & (TT_Wall)) == 0;

        // 是否有箱子陷入墙壁或地图角落
        if (!horPushable && !verPushable) return false;
        else if (horPushable && verPushable) continue;
        // 是否可能划出来
        else if (horPushable)
        {
            int minX = x;
            while (minX - 1 >= 0 && (m.getTileType(minX - 1, y) & TT_Wall) == 0) --minX;

            bool successable = false;
            for (int newX = minX; newX < m.w; ++newX)
            {
                if ((m.getTileType(newX, y) & TT_Wall) != 0) break;
                if ((m.getTileType(newX, y) & TT_Dest) != 0)
                {
                    successable = true; 
                    break;
                }
                if ((y - 1 >= 0 && y + 1 < m.h) && 
                    (m.getTileType(newX, y - 1) & (TT_Wall)) == 0 && 
                    (m.getTileType(newX, y + 1) & (TT_Wall)) == 0)
                {
                    successable = true; 
                    break;
                }
            }
            if (!successable) return false;
        }
        else if (verPushable)
        {
            int minY = y;
            while (minY - 1 >= 0 && (m.getTileType(x, minY - 1) & TT_Wall) == 0) --minY;

            bool successable = false;
            for (int newY = minY; newY < m.w; ++newY)
            {
                if ((m.getTileType(x, newY) & TT_Wall) != 0) break;
                if ((m.getTileType(x, newY) & TT_Dest) != 0)
                {
                    successable = true;
                    break;
                }
                if ((x - 1 >= 0 && x + 1 < m.w) && 
                    (m.getTileType(x - 1, newY) & (TT_Wall)) == 0 && 
                    (m.getTileType(x + 1, newY) & (TT_Wall)) == 0)
                {
                    successable = true; 
                    break;
                }
            }
            if (!successable) return false;
        }
    }

    return true;
}

static void findSolutions(
                          std::string& shortestMoves,
                          std::string& moves, TXZMap& m, TXZMapStateHashMap &existMapStates,
                          int moveLimit)
{
    if ((int)moves.size() > moveLimit) return;

    if (!shortestMoves.empty() && moves.size() >= shortestMoves.size()) return;
    if (m.isAllBoxDestOverlapping())
    {
        shortestMoves = moves;
        return;
    }

    int playerIdx = m.getPlayerId();

    static std::vector<int> mapState;
    mapState.clear();
    getTilesHasTileType(mapState, m, TT_Box);
    mapState.push_back(playerIdx);
    {
        TXZMapStateHashMap::iterator iter = existMapStates.find(mapState);
        if (iter != existMapStates.end()) 
        {
            if (iter->second <= (int)moves.size()) return;
            iter->second = (int)moves.size();
        }
        else
        {
            existMapStates.insert(TXZMapStateHashMap::value_type(mapState, (int)moves.size()));
        }
    }

    if (!quickCheckIsTXZMapSuccessable(m)) 
    {
        existMapStates.find(mapState)->second = 0;
        return;
    }

    TXZMapPathFinder pathFinder(m, playerIdx);

    size_t oldMoveCnt = (int)moves.size();

    std::vector<int> tileIter;
    std::vector<int> boxTiles;
    getTilesHasTileType(boxTiles, m, TT_Box);
    for (int i = 0; i < (int)boxTiles.size(); ++i)
    {
        int boxIdx = boxTiles[i];
        tileIter.clear();
        getAdjacencyTileIter(tileIter, m, boxIdx);
        for (int j = 0; j < (int)tileIter.size(); ++j)
        {
            int adjacencyIdx = tileIter[j];
            if ((m.data[adjacencyIdx] & (TT_Box | TT_Wall)) != 0) continue;

            char dir = idxChangeToDir(boxIdx, adjacencyIdx);

            int oppsAdjacencyIdx = idxAddDir(boxIdx, oppositeDir(dir), m);
            if (oppsAdjacencyIdx == -1) continue;
            if ((m.data[oppsAdjacencyIdx] & (TT_Box | TT_Wall)) != 0) continue;

            std::string path;
            if (oppsAdjacencyIdx != playerIdx)
            {
                path = pathFinder.getPath(oppsAdjacencyIdx);
                if (path.empty()) continue;
            }

            moves += path + dir;
            // 移动箱子
            m.data[boxIdx] &= ~TT_Box;
            m.data[adjacencyIdx] |= TT_Box;
            // 移动玩家
            m.data[playerIdx] &= ~TT_Player;
            m.data[boxIdx] |= TT_Player;

            findSolutions(shortestMoves, moves, m, existMapStates, moveLimit);

            moves.erase(oldMoveCnt);
            // 移动箱子
            m.data[boxIdx] |= TT_Box;
            m.data[adjacencyIdx] &= ~TT_Box;
            // 移动玩家
            m.data[playerIdx] |= TT_Player;
            m.data[boxIdx] &= ~TT_Player;
        }
    }
}

std::string generateSolution(const TXZMap& m, int moveLimit)
{
    std::string shortestMoves, moves;
    TXZMap tempMap(m);
    TXZMapStateHashMap existMapStates;
    findSolutions(shortestMoves, moves, tempMap, existMapStates, moveLimit);
    return shortestMoves;
}