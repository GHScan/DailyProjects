#pragma once

#include "ScanUtil.h"

class FuncHook
{
public:
	// asmInstruAlign是看oldfunc源码中一个指令边界的字节数, 必须不小于7字节
    FuncHook(void *oldFunc, void *newFunc, size_t asmInstruAlign);
    ~FuncHook();

    bool isValid() const;

    template<typename T>
    T getOldFunc(T)
    {
        return Scan::force_cast<T>(m_oldFuncBackup);
    }

private:
    void    *m_oldFunc;
    void    *m_newFunc;
    char     m_oldFuncBackup[32];
    bool     m_isValid;
};
