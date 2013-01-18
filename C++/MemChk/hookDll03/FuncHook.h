#pragma once

#include "ScanUtil.h"

class FuncHook
{
public:
	// asmInstruAlign�ǿ�oldfuncԴ����һ��ָ��߽���ֽ���, ���벻С��7�ֽ�
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
