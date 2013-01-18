#ifndef REG_PARSER_H
#define REG_PARSER_H

#include <string>
#include <memory>
#include <bitset>
#include <vector>

struct RegNode_Charset;
struct RegNode_Capture;
struct RegNode_Repeat;
struct RegNode_Concat;
struct RegNode_Or;
struct IRegNodeVisitor
{
    virtual ~IRegNodeVisitor(){}
    virtual void visit(RegNode_Charset *v) = 0;
    virtual void visit(RegNode_Capture *v) = 0;
    virtual void visit(RegNode_Repeat *v) = 0;
    virtual void visit(RegNode_Concat *v) = 0;
    virtual void visit(RegNode_Or *v) = 0;
};

const int REPEAT_MAX = 1000000;
struct IRegNode
{
    virtual ~IRegNode(){}
    virtual void acceptVisitor(IRegNodeVisitor *v) = 0;
};
typedef std::shared_ptr<IRegNode> RegNodePtr;
struct RegNode_Charset:
    public IRegNode
{
    std::bitset<128> sets;
    virtual void acceptVisitor(IRegNodeVisitor *v) { v->visit(this);}
};
struct RegNode_Capture:
    public IRegNode
{
    int capID;
    RegNodePtr node;
    RegNode_Capture(const RegNodePtr& n): node(n){}
    virtual void acceptVisitor(IRegNodeVisitor *v) { v->visit(this);}
};
struct RegNode_Repeat:
    public IRegNode
{
    int min, max;
    bool gready;
    RegNodePtr node;
    RegNode_Repeat(int min, int max, bool gready, const RegNodePtr& n): node(n)
    { this->min = min, this->max = max, this->gready = gready;}
    virtual void acceptVisitor(IRegNodeVisitor *v) { v->visit(this);}
};
struct RegNode_Concat:
    public IRegNode
{
    RegNodePtr left, right;
    RegNode_Concat(const RegNodePtr& l, const RegNodePtr& r):left(l), right(r){}
    virtual void acceptVisitor(IRegNodeVisitor *v) { v->visit(this);}
};
struct RegNode_Or:
    public IRegNode
{
    RegNodePtr left, right;
    RegNode_Or(const RegNodePtr& l, const RegNodePtr& r):left(l), right(r){}
    virtual void acceptVisitor(IRegNodeVisitor *v) { v->visit(this);}
};

class RegParser
{
public:
    RegParser(const std::string& src);
    ~RegParser();
    RegNodePtr& getNode();
private:
    class RegParserImpl *m_impl;
};

class RegNodeVisitor_Printer
{
public:
    RegNodeVisitor_Printer();
    ~RegNodeVisitor_Printer();
    std::string apply(RegNodePtr node);
private:
    class RegNodeVisitor_PrinterImpl *m_impl;
};

class RegNodeVisitor_LogicPrinter
{
public:
    RegNodeVisitor_LogicPrinter();
    ~RegNodeVisitor_LogicPrinter();
    std::string apply(RegNodePtr node);
private:
    class RegNodeVisitor_LogicPrinterImpl *m_impl;
};

#endif
