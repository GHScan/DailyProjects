
#ifndef BE_x86_REGISTER_H
#define BE_x86_REGISTER_H

struct BESymbol;
struct BEVariable;

enum BEx86RegisterType {
    x86RT_EAX = 1 << 0,
    x86RT_EBX = 1 << 1,
    x86RT_ECX = 1 << 2,
    x86RT_EDX = 1 << 3,
    x86RT_ESI = 1 << 4,
    x86RT_EDI = 1 << 5,
    x86RT_ESP = 1 << 6,
    x86RT_EBP = 1 << 7,

    x86RT_Count = 8,
};

struct BEx86Register {
    BEx86Register(BEx86RegisterType _type): type(_type){}
    const BEx86RegisterType type;
    map<BESymbol*, BEVariable*> loadedVars;
};

class BEx86RegisterManager {
public:
    BEx86RegisterManager();
    BEx86Register* getRegs() { return &m_regs[0]; }

private:
    vector<BEx86Register> m_regs;
};

#endif
