
#ifndef BE_x86_JIT_ENGINE_H
#define BE_x86_JIT_ENGINE_H

class BEx86FileBuilder;

class BEx86JITEngine: public Noncopyable {
public:
    BEx86JITEngine(BEx86FileBuilder* fileBuilder);
    ~BEx86JITEngine();
    void* getSymbol(const string &name);
    void dumpCode(ostream &so);
private:
    class BEx86JITEngineImpl *m_impl;
};

#endif
