#include "StdAfx.h"

#include <cassert>

#include "WinSetsTemplate.h"

WinSetsTemplate::WinSetsTemplate(void)
{
}

WinSetsTemplate::~WinSetsTemplate(void)
{
}

void WinSetsTemplate::enumNxNWinSets(int n)
{
    int maxCntPerRow = n - 5 + 1;
    int cnt = 
        (maxCntPerRow * n) * 2 + 
        (maxCntPerRow + maxCntPerRow * (maxCntPerRow - 1)) * 2;
    onBeginEnumeration(cnt);

    int winID = 0;

    for (int row = 0; row < n; ++row)
    {
        for (int beginColumn = 0; beginColumn <= n - 5; ++beginColumn)
        {
            for (int i = 0; i < 5; ++i)
            {
                onWinSet(winID, row, beginColumn + i);
            }

            ++winID;

            // 对称性
            for (int i = 0; i < 5; ++i)
            {
                onWinSet(winID, beginColumn + i, row);
            }

            ++winID;
        }
    }

    for (int row = 0; row <= n - 5; ++row)
    {
        for (int i = 0; i < 5; ++i)
        {
            onWinSet(winID, row + i, row + i);
        }

        ++winID;

        // 对称性
        for (int i = 0; i < 5; ++i)
        {
            onWinSet(winID, row + i, n - 1 - row - i);
        }

        ++winID;
    }

    for (int row = 1; row <= n - 5; ++row)
    {
        for (int row2 = row; row2 <= n - 5; ++row2)
        {
            for (int i = 0; i < 5; ++i)
            {
                onWinSet(winID, row2 + i, row2 - row + i);
            }

            ++winID;

            // 主对角线对称

            for (int i = 0; i < 5; ++i)
            {
                onWinSet(winID, row2 - row + i, row2 + i);
            }

            ++winID;

            // 交x对称

            for (int i = 0; i < 5; ++i)
            {
                onWinSet(winID, row2 + i, n - 1 - (row2 - row) - i);
            }

            ++winID;

            // 和上一个关于此对角线对称

            for (int i = 0; i < 5; ++i)
            {
                onWinSet(winID, row2 - row + i, n - 1 - row2 - i);
            }

            ++winID;
        }
    }

    assert(winID == cnt);

    onEndEnumeration();
}