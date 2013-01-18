#include "pch.h" 

#include <string.h>

#include <string>
#include <sstream>
#include <memory>
#include <map>

#include "common.h"

// xml syntax
/*
    root = elem
    elems = elem elems | e
    elem = fullelem | simpleelem | comment | declare | text
    fullelem = beginelem elems endelem
    beginelem = <elemtag >
    endelem = </elemtag>
    simpleelem = < elemtag />
    comment = <!-- text -->
    declare = <? elemtag ?>
    elemtag = .*
    text = \S.*\S?
*/

// elemtag syntax
/*
   elemtag = id attris
   attris = attr attris | e
   attr = id=str
   id = \a[-:\w]*
   str = ".*"
*/

struct Node_Text;
struct Node_Comment;
struct Node_Element;
struct Node_Declare;
struct INodeVisitor
{
    virtual ~INodeVisitor() {}
    virtual void visit(Node_Text *v) = 0;
    virtual void visit(Node_Comment *v) = 0;
    virtual void visit(Node_Declare *v) = 0;
    virtual void visit(Node_Element *v) = 0;
};

struct INode
{
    virtual ~INode(){}
    virtual void acceptVisitor(INodeVisitor *v) = 0;
};
typedef std::shared_ptr<INode> NodePtr;
typedef std::vector<std::pair<std::string, std::string>> AttriMap;
typedef std::vector<NodePtr> NodePtrList;
struct Node_Text:
    public INode
{
    std::string s;
    virtual void acceptVisitor(INodeVisitor *v) { v->visit(this); }
};
struct Node_Comment:
    public INode
{
    std::string s;
    virtual void acceptVisitor(INodeVisitor *v) { v->visit(this); }
};
struct Node_Element:
    public INode
{
    std::string tag;
    AttriMap attris;
    NodePtrList children;
    virtual void acceptVisitor(INodeVisitor *v) { v->visit(this); }
};
struct Node_Declare:
    public INode
{
    std::string tag;
    AttriMap attris;
    virtual void acceptVisitor(INodeVisitor *v) { v->visit(this); }
};

// ==================== elem tag parse
namespace ElemTagSyntax
{

enum TokenType
{
    TT_notation = 100,
    TT_id,
    TT_str,
};

class Parser:
    public LexicalAnalysiser
{
public:
    Parser(const std::string& src, std::string& id, AttriMap& m)
    {
        lexicalAnalysis<TT_notation, TT_str>("nonfile", src, false);
        elemtag(id, m);
    }
private:
    void elemtag(std::string& id, AttriMap& m)
    {
        if (tryConsumeToken(TT_id)) {
            id = getPreviewToken().value;
            attris(m);
        }
    }
    void attris(AttriMap& m)
    {
        std::string k, v;
        while (attri(k, v)) m.push_back(std::make_pair(k, v));
    }
    bool attri(std::string& k, std::string& v)
    {
        backupTokenPos();
        if (tryConsumeToken(TT_id)) {
            k = getPreviewToken().value;
            if (tryConsumeToken(TT_notation, "=")) {
                consumeToken(TT_str);
                v = getPreviewToken().value;
                v = v.substr(1, v.size() - 2);
                discardBackupTokenPos();
                return true;
            }
        }
        restoreTokenPos();
        return false;
    }
};

}
template<>
const char* matchLexeme<ElemTagSyntax::TT_notation>(const char* src)
{
    if (src[0] && strchr("=", src[0])) return ++src;
    return NULL;
}
template<>
const char* matchLexeme<ElemTagSyntax::TT_id>(const char* src)
{
    if (!isalpha(src[0])) return NULL;
    ++src;
    while (isalpha(src[0]) || isdigit(src[0]) || (src[0] && strchr(":-_", src[0]))) ++src;
    return src;
}
template<>
const char* matchLexeme<ElemTagSyntax::TT_str>(const char* src)
{
    if (!strchr("\"'", src[0])) return NULL;
    char c = *src;
    while (*++src != c);
    return ++src;
}

// ==================== xml parse

enum XmlTokenType
{
    XTT_text = 1,
    XTT_beginelem,
    XTT_simpleelem,
    XTT_comment,
    XTT_declare,
    XTT_endelem,
};
const char* skipElemTag(const char *src, const char *exclude)
{
    bool isInStr = false;
    for (; isInStr || !strchr(exclude, src[0]); ++src) {
        if (strchr("\"'", src[0])) isInStr = !isInStr;
    }
    return src;
}
template<>
const char* matchLexeme<XTT_text>(const char* src)
{
    if (src[0] == '<') return NULL;
    while (*++src != '<');
    return src;
}
template<>
const char* matchLexeme<XTT_comment>(const char* src)
{
    if (strncmp(src, "<!--", 4)) return NULL;
    src += 4;
    while (strncmp(src, "-->", 3)) ++src;
    return src += 3;
}
template<>
const char* matchLexeme<XTT_declare>(const char* src)
{
    if (!(src[0] == '<' && src[1] == '?')) return NULL;
    src = skipElemTag(src + 2, "?>");
    if (src[0] != '?' || src[1] != '>') return NULL;
    return src += 2;
}
template<>
const char* matchLexeme<XTT_simpleelem>(const char* src)
{
    if (src[0] != '<') return NULL;
    src = skipElemTag(src + 1, "/>");
    if (src[0] != '/' || src[1] != '>') return NULL;
    return src += 2;
}
template<>
const char* matchLexeme<XTT_beginelem>(const char* src)
{
    if (src[0] != '<') return NULL;
    return skipElemTag(src + 1, ">") + 1;
}
template<>
const char* matchLexeme<XTT_endelem>(const char* src)
{
    if (!(src[0] == '<' && src[1] == '/')) return NULL;
    return skipElemTag(src + 2, ">") + 1;
}

class XmlParser:
    public LexicalAnalysiser
{
public:
    XmlParser(const std::string& fname, const std::string& src)
    {
        lexicalAnalysis<XTT_text, XTT_endelem>(fname, src, false);
        root();
    }
    NodePtrList& getRoot() { return m_root; }
private:
    void root()
    {
        elems(m_root);
    }
    void elems(NodePtrList &list)
    {
        while (NodePtr e = elem()) list.push_back(e);
    }
    NodePtr elem()
    {
        NodePtr e = simpleelem();
        if (e != NULL) return e;
        if (e = comment()) return e;
        if (e = declare()) return e;
        if (e = text()) return e;
        if (e = fullelem()) return e;
        return e;
    }
    NodePtr fullelem()
    {
        if (tryConsumeToken(XTT_beginelem)) {
            Node_Element *e = new Node_Element();
            NodePtr r(e);
            {
                const std::string& s = getPreviewToken().value;
                ElemTagSyntax::Parser(s.substr(1, s.size() - 2), e->tag, e->attris);
            } 

            elems(e->children);

            consumeToken(XTT_endelem);
            {
                const std::string& s = getPreviewToken().value;
                std::string tag;
                AttriMap m;
                ElemTagSyntax::Parser(s.substr(2, s.size() - 3), tag, m);
                PARSE_ASSERT(tag == e->tag && m.empty());
            }
            return r;
        }
        return NodePtr();
    }
    NodePtr simpleelem()
    {
        if (tryConsumeToken(XTT_simpleelem)) {
            Node_Element *e = new Node_Element();
            NodePtr r(e);
            const std::string &s = getPreviewToken().value;
            ElemTagSyntax::Parser(s.substr(1, s.size() - 3), e->tag, e->attris);
            return r;
        }
        return NodePtr();
    }
    NodePtr comment()
    {
        if (tryConsumeToken(XTT_comment)) {
            Node_Comment *e = new Node_Comment();
            NodePtr r(e);
            const std::string &s = getPreviewToken().value;
            e->s = s.substr(4, s.size() - 7);
            return r;
        }
        return NodePtr();
    }
    NodePtr declare()
    {
        if (tryConsumeToken(XTT_declare)) {
            Node_Declare *e = new Node_Declare();
            NodePtr r(e);
            const std::string &s = getPreviewToken().value;
            ElemTagSyntax::Parser(s.substr(2, s.size() - 4), e->tag, e->attris);
            return r;
        }
        return NodePtr();
    }
    NodePtr text()
    {
        if (tryConsumeToken(XTT_text)) {
            Node_Text *e = new Node_Text();
            NodePtr r(e);
            const char *start = getPreviewToken().value.c_str();
            const char *last = start + getPreviewToken().value.size() - 1;
            while (start <= last && isspace(start[0])) ++start;
            while (start <= last && isspace(last[0])) --last;
            e->s.assign(start, last + 1);
            return r;
        }
        return NodePtr();
    }
private:
    NodePtrList m_root;
};

void printTab(std::ostream& so, int n)
{
    for (int i = 0; i < n; ++i) so << '\t';
}

class NodeVisitor_Xml:
    public INodeVisitor
{
public:
    std::string apply(NodePtrList &root)
    {
        m_so.str("");
        m_d = 0;
        for (NodePtrList::iterator iter = root.begin(); iter != root.end(); ++iter) {
            (*iter)->acceptVisitor(this);
        }
        return m_so.str().substr(1);
    }
private:
    virtual void visit(Node_Text *v)
    {
        m_so << '\n'; printTab(m_so, m_d);
        m_so << v->s;
    }
    virtual void visit(Node_Comment *v)
    {
        m_so << '\n'; printTab(m_so, m_d);
        m_so << "<!--" << v->s << "-->";
    }
    virtual void visit(Node_Declare *v)
    {
        m_so << '\n'; printTab(m_so, m_d);
        m_so << "<?" << v->tag;
        printAttris(v->attris);
        m_so << "?>";
    }
    virtual void visit(Node_Element *v)
    {
        m_so << '\n'; printTab(m_so, m_d);
        m_so << "<" << v->tag;
        printAttris(v->attris);
        if (v->children.empty()) {
            m_so << "/>";
        }
        else {
            m_so << ">";
            ++m_d;
            for (NodePtrList::iterator iter = v->children.begin(); iter != v->children.end(); ++iter) {
                (*iter)->acceptVisitor(this);
            }
            --m_d;
            m_so << '\n'; printTab(m_so, m_d);
            m_so << "</" << v->tag << ">";
        }
    }
private:
    void printAttris(const AttriMap& m)
    {
        for (AttriMap::const_iterator iter = m.begin(); iter != m.end(); ++iter) {
            m_so << ' ';
            m_so << iter->first;
            m_so << "=\"";
            m_so << iter->second;
            m_so << '"';
        }
    }
private:
    std::ostringstream m_so;
    int m_d;
};

class NodeVisitor_Lua:
    public INodeVisitor
{
public:
    std::string apply(NodePtrList &root)
    {
        m_so.str("");
        m_d = 0;
        printNodes(root);
        return m_so.str();
    }
private:
    virtual void visit(Node_Text *v)
    {
        printKV("type", "text");
        printKV("str", v->s);
    }
    virtual void visit(Node_Comment *v)
    {
        printKV("type", "comment");
        printKV("str", v->s);
    }
    virtual void visit(Node_Declare *v)
    {
        printKV("type", "declare");
        printKV("tag", v->tag);
        printAttris(v->attris);
    }
    virtual void visit(Node_Element *v)
    {
        printKV("type", "element");
        printKV("tag", v->tag);
        printAttris(v->attris);
        if (!v->children.empty()) {
            printTab(m_so, m_d); m_so << "children = ";
            printNodes(v->children);
            m_so << ",\n";
        }
    }
private:
    void printNodes(NodePtrList& list)
    {
        m_so << "{\n";
        ++m_d;
        for (NodePtrList::iterator iter = list.begin();
                iter != list.end(); 
                ++iter) {
            printTab(m_so, m_d); m_so << "{\n";
            ++m_d;
            (*iter)->acceptVisitor(this);
            --m_d;
            printTab(m_so, m_d); m_so << "},\n";
        }
        --m_d;
        printTab(m_so, m_d); m_so << "}";
    }
    void printAttris(const AttriMap& m)
    {
        if (m.empty()) return;
        printTab(m_so, m_d); m_so << "attris = {\n";
        ++m_d;
        for (AttriMap::const_iterator iter = m.begin(); iter != m.end(); ++iter) {
            printKV(iter->first, iter->second);
        }
        --m_d;
        printTab(m_so, m_d); m_so << "},\n";
    }
    void printKV(const std::string& k, const std::string& v)
    {
        printTab(m_so, m_d); 
        m_so << "['" << k << "'] = '" << v << "',\n";
    }
private:
    std::ostringstream m_so;
    int m_d;
};

int main()
{
    try 
    {
        std::string fname = "1.xml";
        XmlParser p(fname, readFile(fname));

        std::ofstream("2.xml") << NodeVisitor_Xml().apply(p.getRoot());
        std::ofstream("3.lua") << NodeVisitor_Lua().apply(p.getRoot());
    }
    catch (const std::exception& e)
    {
        cout << "Exception : " << e.what() << endl;
    }
}
