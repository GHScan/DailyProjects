#pragma once

#include <vector>
#include <string>
#include <sstream>
#include <iomanip>

const int MAX_MAP_SIZE = 65535;

enum TileType
{
    TT_Dest     = 0x01,
    TT_Wall     = 0x02,
    TT_Box      = 0x10,
    TT_Player   = 0x20,
};

struct TXZMap
{
    std::vector<int> data;
    int w, h;
    TXZMap():
    w(0), h(0){}

    bool getPlayerPos(int &x, int &y) const
    {
        for (int i = 0; i < (int)data.size(); ++i)
        {
            if ((data[i] & TT_Player) != 0)
            {
                y = i / w, x = i % w;
                return true;
            }
        }
        return false;
    }
    int getPlayerId() const
    {
        for (int i = 0; i < (int)data.size(); ++i) if ((data[i] & TT_Player) != 0) return i;
        return -1;
    }
    int getTileCnt(TileType tt) const
    {
        int r = 0;
        for (size_t i = 0; i < data.size(); ++i)
        {
            if ((data[i] & tt) != 0) ++r;
        }
        return r;
    }
    bool isAllBoxDestOverlapping() const
    {
        int mask = TT_Dest | TT_Box;
        for (size_t i = 0; i < data.size(); ++i)
        {
            if ((data[i] & mask) != 0 && data[i] != mask) return false;
        }
        return true;
    }
    TileType getTileType(int x, int y) const
    {
        int idx = y * w + x;
        ASSERT_INSIDE_N(idx, (int)data.size());
        return (TileType)data[idx];
    }
    void setTileType(int x, int y, TileType tt)
    {
        int idx = y * w + x;
        ASSERT_INSIDE_N(idx, (int)data.size());
        data[idx] = tt;
    }
    void removeTileType(int x, int y, TileType tt)
    {
        int idx = y * w + x;
        ASSERT_INSIDE_N(idx, (int)data.size());
        data[idx] &= ~tt;
    }
    void addTileType(int x, int y, TileType tt)
    {
        int idx = y * w + x;
        ASSERT_INSIDE_N(idx, (int)data.size());
        data[idx] |= tt;
    }
    bool checkTXZMap() const
    {
        if (w > MAX_MAP_SIZE || h > MAX_MAP_SIZE) return false;
        if (w * h != data.size()) return false;
        if (getTileCnt(TT_Player) != 1) return false;
        if (getPlayerId() == -1) return false;
        if (getTileCnt(TT_Box) != getTileCnt(TT_Dest)) return false;
        return true;
    }
};

inline char tileTypeToChar(TileType tt)
{
    switch (tt)
    {
    case 0: return '.';
    case TT_Wall: return '#';
    case TT_Dest: return 'd';
    case TT_Box: return 'b';
    case TT_Player: return 'p';
    case TT_Dest | TT_Box: return 'B';
    case TT_Dest | TT_Player: return 'P';
    default: break;
    }
    verify(0); return 0;
}
inline int charToTileType(char c)
{
    switch (c)
    {
    case '.': return 0;
    case '#': return TT_Wall;
    case 'd': return TT_Dest;
    case 'b': return TT_Box;
    case 'p': return TT_Player;
    case 'B': return TT_Box | TT_Dest;
    case 'P': return TT_Player | TT_Dest;
    default:break;
    }
    verify(0); return 0;
}

inline std::ostream& operator << (std::ostream& so, const TXZMap& m)
{
    verify(m.data.size() == m.w * m.h);
    for (int y = 0; y < m.h; ++y)
    {
        for (int x = 0; x < m.w; ++x)
        {
            so << tileTypeToChar(m.getTileType(x, y));
        }
        so << "\n";
    }
    return so;
}

inline std::istream& operator >> (std::istream& si, TXZMap& m)
{
    m.data.clear();
    m.w = m.h = 0;
    for (std::string line; getline(si, line);)
    {
        if (line.empty()) continue;

        if (m.w == 0) m.w = (int)line.size();
        else verify(m.w == (int)line.size());

        for (size_t i = 0; i < line.size(); ++i)
        {
            m.data.push_back(charToTileType(line[i]));
        }

        ++m.h;
    }
    return si;
}

struct TXZMapRecord:
    public TXZMap
{
    int bestMoveCnt;
};

inline std::ostream& operator << (std::ostream& so , const TXZMapRecord& rcd)
{
    so << rcd.bestMoveCnt << "\n";
    so << (const TXZMap&)rcd;
    return so;
}

inline std::istream& operator >> (std::istream& si, TXZMapRecord& rcd)
{
    si >> rcd.bestMoveCnt;
    si >> (TXZMap&)rcd;
    return si;
}

inline char playerMoveOffToChar(QPoint pt)
{
    verify(qAbs(pt.x()) + qAbs(pt.y()) == 1);
    if (pt.x() == -1) return 'l';
    else if (pt.x() == 1) return 'r';
    else if (pt.y() == -1) return 'u';
    else if (pt.y() == 1) return 'd';
    else 
    {
        verify(0);
        return 0;
    }
}
inline QPoint charToPlayerMoveOff(char c)
{
    if (isupper(c)) c = c + 0x20;
    if (c == 'l') return QPoint(-1, 0);
    else if (c == 'r') return QPoint(1, 0);
    else if (c == 'u') return QPoint(0, -1);
    else if (c == 'd') return QPoint(0, 1);
    else 
    {
        verify(0);
        return QPoint();
    }
}

struct TXZReplay
{
    TXZMap map;
    std::vector<QPoint> playerTrace;
};

inline std::ostream& operator << (std::ostream& so , const TXZReplay& replay)
{
    so << "=";
    for (size_t i = 0; i < replay.playerTrace.size(); ++i)
    {
        so << playerMoveOffToChar(replay.playerTrace[i]);
    }
    so << "\n";

    so << replay.map;
    return so;
}

inline std::istream& operator >> (std::istream& si, TXZReplay& replay)
{
    replay.playerTrace.clear();
    while (si)
    {
        char head = si.get();
        si.putback(head);
        if (head != '=') break;
        std::string line;
        getline(si, line);
        line = line.substr(1);

        for (size_t i = 0; i < line.size(); ++i)
        {
            replay.playerTrace.push_back(charToPlayerMoveOff(line[i]));
        }
    }

    si >> replay.map;
    return si;
}