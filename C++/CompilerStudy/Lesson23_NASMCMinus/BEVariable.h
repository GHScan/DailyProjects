
#ifndef BE_VARIABLE_H
#define BE_VARIABLE_H

struct BESymbol;
struct BEx86Register;

struct BEVariable {
    BEx86Register *reg;
    BESymbol *symbol;
    bool isInMemory;
};

class BEVariableManager {
public:
    ~BEVariableManager();
    BEVariable* createVariable(BESymbol *symbol, bool isInMemory, BEx86Register *reg);
    void destroy(const string &name);
    BEVariable* get(const string &name);

private:
    map<string, BEVariable> m_vars;
};

#endif
