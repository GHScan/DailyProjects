
#ifndef SYMBOL_TABLE_H
#define SYMBOL_TABLE_H

struct IType;

struct SymbolDescrib
{
    int idx;
    int off;
    IType *type;
};
class SymbolTable
{
public:
    SymbolTable(SymbolTable *parent): 
        m_parent(parent), m_off(parent == NULL ? 0 : parent->getOffset()){}
    ~SymbolTable(){}

    int getInitOffset() const { return m_parent == NULL ? 0 : m_parent->getOffset(); }
    int getOffset() const { return m_off; }
    
    void addSymbol(const string& name, IType *type);
    const SymbolDescrib* getSymbol(const string& name);
    const SymbolDescrib* getSymbol(int idx);
private:
    SymbolTable *m_parent;
    int m_off;
    map<string, SymbolDescrib> m_name2Symbol;
    vector<SymbolDescrib*> m_id2Symbol;
};
typedef shared_ptr<SymbolTable> SymbolTablePtr;

class SymbolTableStack
{
public:
    void push();
    void pop();
    int getOffset();

    void addSymbol(const string& name, IType *type);
    const SymbolDescrib* getSymbol(const string& name);
private:
    vector<SymbolTablePtr> m_stack;
};

class SymbolTableManager
{
public:
    SymbolTableManager()
    {
        m_global.reset(new SymbolTable(NULL));
    }
    ~SymbolTableManager(){}
    static SymbolTableManager* instance()
    {
        static SymbolTableManager s_ins;
        return &s_ins;
    }

    const SymbolTablePtr& global() { return m_global;}
    SymbolTableStack* stack() { return &m_stack;}
    void addTypeTable(IType *type, const SymbolTablePtr& table)
    {
        m_typeTables[type] = table;
    }
    const SymbolTablePtr& getTypeTable(IType *type) { return m_typeTables[type]; }
private:
    SymbolTablePtr m_global;
    SymbolTableStack m_stack;
    map<IType*, SymbolTablePtr> m_typeTables;
};

#endif
