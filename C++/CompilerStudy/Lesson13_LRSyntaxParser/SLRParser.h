
#ifndef SLRPARSER_H
#define SLRPARSER_H

#include "LRParser.h"

class SLRActionTable: 
    public IActionTable
{
public:
    virtual Action getAction(int state, const string& term);
    virtual int getStateCount();
    map<int, map<string, Action> > actionMap;
};
class SLRGotoTable:
    public IGotoTable
{
public:
    virtual int getNextState(int state, const string& nonTerm);
    map<int, map<string, int> > gotoMap;
};

class SLRParserTable:
    public IParserTable
{
public:
    SLRParserTable();
    ~SLRParserTable();
private:
    virtual IActionTable* getActionTable();
    virtual IGotoTable* getGotoTable();
    virtual const BNFInstance* getBNF();

    virtual void init(const BNFInstance& bnf);
    virtual int getStartState();
private:
    SLRActionTable *m_actionTable;
    SLRGotoTable *m_gotoTable;
    const BNFInstance *m_bnf;
};

#endif
