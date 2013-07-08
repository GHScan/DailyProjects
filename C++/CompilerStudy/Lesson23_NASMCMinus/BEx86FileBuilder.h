
#ifndef BE_x86_FILE_BUILDER_H
#define BE_x86_FILE_BUILDER_H

class BESymbolTable;
class BEConstantPool;
class BEx86FunctionBuilder;
struct BEType;

struct BEx86BuildConfig {
    int stackAlignment;
    BEx86BuildConfig(): stackAlignment(4) {}
};

class BEx86FileBuilder: public Noncopyable {
public:
    BEx86FileBuilder();
    ~BEx86FileBuilder();

    BEx86BuildConfig* getBuildConfig() { return &m_buildConfig; }

    BEx86FunctionBuilder* createFunctionBuilder(const string &name);
    BEx86FunctionBuilder* getFunctionBuilder(const string &name);

    BEConstantPool* getConstantPool() { return m_constantPool; }
    BESymbolTable* getGlobalSymbolTable() { return m_globalSymbolTable; }

    void setAsExternSymbol(const string &name);
    bool isExternSymbol(const string &name);
private:
    BEConstantPool *m_constantPool;
    BESymbolTable *m_globalSymbolTable;
    map<string, BEx86FunctionBuilder*> m_funcBuilders;
    set<string> m_externSymbols;
    BEx86BuildConfig m_buildConfig;
};

typedef shared_ptr<BEx86FileBuilder> BEx86FileBuilderPtr;

#endif
