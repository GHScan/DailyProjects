
#ifndef LRPARSER_H
#define LRPARSER_H

#include "BNFParser.h"

struct Action
{
    enum Type
    {
        T_Shift,
        T_Reduce,
    };
    Type type : 8;
    int value : 24;
    Action(){}
    Action(Type t, int v): type(t), value(v){}
};

struct IActionTable
{
    virtual ~IActionTable(){}
    virtual int getStateCount() = 0;
    virtual Action getAction(int state, const string& term) = 0;
};
struct IGotoTable
{
    virtual ~IGotoTable() {}
    virtual int getNextState(int state, const string& nonTerm) = 0;
};

struct IParserTable
{
    virtual ~IParserTable(){}
    static IParserTable* create(const string& type);
    virtual IActionTable* getActionTable() = 0;
    virtual IGotoTable* getGotoTable() = 0;
    virtual const BNFInstance* getBNF() = 0;

    virtual void init(const BNFInstance& bnf) = 0;
    virtual int getStartState() = 0;
};

struct ILROutput
{
    virtual ~ILROutput(){}
    virtual void onBeginBuild() = 0;
    virtual void onEndBuild(int stateCnt) = 0;
    virtual void onBeginParse() = 0;
    virtual void onReduce(const Production& p) = 0;
    virtual void onShift(const string& term) = 0;
    virtual void onEndParse(bool success) = 0;
};

class LRParser
{
public:
    LRParser(const string& type, const BNFInstance& bnf, ILROutput *output);
    ~LRParser();
    void parse(const string& src);
private:
    IParserTable *m_table;
    ILROutput *m_output;
};

#endif
