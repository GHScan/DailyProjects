
#ifndef CLRPARSER_H
#define CLRPARSER_H

#include "LRParser.h"

class CLRActionTable: 
    public IActionTable
{
public:
    virtual Action getAction(int state, const string& term);
    virtual int getStateCount();
    map<int, map<string, Action> > actionMap;
};
class CLRGotoTable:
    public IGotoTable
{
public:
    virtual int getNextState(int state, const string& nonTerm);
    map<int, map<string, int> > gotoMap;
};

class CLRParserTable:
    public IParserTable
{
public:
    CLRParserTable();
    ~CLRParserTable();
private:
    virtual IActionTable* getActionTable();
    virtual IGotoTable* getGotoTable();
    virtual const BNFInstance* getBNF();

    virtual void init(const BNFInstance& bnf);
    virtual int getStartState();
private:
    CLRActionTable *m_actionTable;
    CLRGotoTable *m_gotoTable;
    const BNFInstance *m_bnf;
};

#endif
