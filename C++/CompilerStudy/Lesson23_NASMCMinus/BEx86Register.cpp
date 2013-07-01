
#include "pch.h"
#include "BEx86Register.h"

BEx86RegisterManager::BEx86RegisterManager() {
    for (int i = 0; i < x86RT_Count; ++i) {
        m_regs.push_back(BEx86Register((BEx86RegisterType)(1 << i)));
    }
}
