#ifndef LALRPARSER_H
#define LALRPARSER_H

#include "LRParser.h"

typedef vector<vector<Action> > LALRStateActionTable;

class LALRActionTable: 
    public IActionTable
{
public:
    virtual Action getAction(int state, const string& term);
    virtual int getStateCount();
    map<string, int> term2ID;
    LALRStateActionTable *table;
};
class LALRGotoTable:
    public IGotoTable
{
public:
    virtual int getNextState(int state, const string& nonTerm);
    map<string, int> nonTerm2ID;
    LALRStateActionTable *table;
};

class LALRParserTable:
    public IParserTable
{
public:
    LALRParserTable();
    ~LALRParserTable();
private:
    virtual IActionTable* getActionTable();
    virtual IGotoTable* getGotoTable();
    virtual const BNFInstance* getBNF();

    virtual void init(const BNFInstance& bnf);
    virtual int getStartState();
private:
    LALRStateActionTable m_table;
    LALRActionTable *m_actionTable;
    LALRGotoTable *m_gotoTable;
    const BNFInstance *m_bnf;
};

#endif
