
#ifndef REGPARSER_H
#define REGPARSER_H

struct RegNode_Charset;
struct RegNode_Concat;
struct RegNode_Or;
struct RegNode_Repeat;
struct RegNode_Capture;
struct IRegNodeVisitor
{
    virtual ~IRegNodeVisitor(){}
    virtual void visit(const RegNode_Charset* node) = 0;
    virtual void visit(const RegNode_Concat* node) = 0;
    virtual void visit(const RegNode_Or* node) = 0;
    virtual void visit(const RegNode_Repeat* node) = 0;
    virtual void visit(const RegNode_Capture* node) = 0;
};

struct IRegNode
{
    virtual ~IRegNode(){}
    virtual void acceptVisitor(IRegNodeVisitor *v) = 0;
};
typedef shared_ptr<IRegNode> RegNodePtr;

struct RegNode_Charset:
    public IRegNode
{
    bool chars[256];
    void inverse() { for (auto &b : chars) b = !b; }
    int count() const 
    {
        int n = 0;
        for (auto b : chars) n += b ? 1 : 0;
        return n;
    }
    RegNode_Charset() { memset(chars, 0, sizeof(chars)); }
    virtual void acceptVisitor(IRegNodeVisitor *v) { v->visit(this); }
};
struct RegNode_Concat:
    public IRegNode
{
    RegNodePtr left, right;
    RegNode_Concat(const RegNodePtr& l, const RegNodePtr& r): left(l), right(r){}
    virtual void acceptVisitor(IRegNodeVisitor *v) { v->visit(this); }
};
struct RegNode_Or:
    public IRegNode
{
    RegNodePtr left, right;
    RegNode_Or(const RegNodePtr& l, const RegNodePtr &r): left(l), right(r){}
    virtual void acceptVisitor(IRegNodeVisitor *v) { v->visit(this); }
};
struct RegNode_Repeat:
    public IRegNode
{
    RegNodePtr node;
    int min, max;
    RegNode_Repeat(const RegNodePtr& n, int _min, int _max): node(n), min(_min), max(_max){}
    static const int MAX_REPEAT = 1 << 30;
    virtual void acceptVisitor(IRegNodeVisitor *v) { v->visit(this); }
};
struct RegNode_Capture:
    public IRegNode
{
    RegNodePtr node;
    RegNode_Capture(const RegNodePtr& n): node(n){}
    virtual void acceptVisitor(IRegNodeVisitor *v) { v->visit(this); }
};

class RegParser
{
public:
    RegParser(const string& src);
    const RegNodePtr &getRoot() const { return m_root;}
private:
    RegNodePtr m_root;
};

class RegNodeVisitor_Printer:
    public IRegNodeVisitor
{
public:
    string apply(const RegNodePtr node);
private:
    virtual void visit(const RegNode_Charset* node);
    virtual void visit(const RegNode_Concat* node);
    virtual void visit(const RegNode_Or* node);
    virtual void visit(const RegNode_Repeat* node);
    virtual void visit(const RegNode_Capture* node);
private:
    string m_str;
};

#endif
