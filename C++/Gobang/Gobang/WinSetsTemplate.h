#pragma once

class WinSetsTemplate
{
public:
    WinSetsTemplate(void);
    ~WinSetsTemplate(void);

    void enumNxNWinSets(int n);

protected:
    virtual void onBeginEnumeration(int winSetCnt) = 0;
    virtual void onWinSet(int winSetID, int x, int y) = 0;
    virtual void onEndEnumeration() = 0;
};
