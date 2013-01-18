#include "StdAfx.h"

#include <windows.h>

#include "FuncHook.h"

bool writeToMemory(void *ptr, char *buf, size_t size)
{
    DWORD oldProtect;
    if (VirtualProtect(ptr, size, PAGE_READWRITE, &oldProtect))
    {
        WriteProcessMemory(GetCurrentProcess(), ptr, buf, size, NULL);
        VirtualProtect(ptr, size, oldProtect, NULL);
        return true;
    }
    return false;
}

bool readFromMemory(void *ptr, char *buf, size_t size)
{   
    SIZE_T readedBytes = 0;
    ReadProcessMemory(GetCurrentProcess(), ptr, buf, size, &readedBytes);
    return readedBytes == size;
}


FuncHook::FuncHook(void *oldFunc, void *newFunc, size_t headerAsmAlign):
m_isValid(false)
{
    assert(headerAsmAlign >= 7);

    m_oldFunc = oldFunc;
    m_newFunc = newFunc;

    // B8 03 20 00 00   mov         eax,2003h 
    // FF E0            jmp         eax  
    char jmp2func[7];

    jmp2func[0] = 0xB8;
    *(void**)&jmp2func[1] = newFunc;
    jmp2func[5] = 0xFF;
    jmp2func[6] = 0xE0;

    if (readFromMemory(oldFunc, m_oldFuncBackup, headerAsmAlign))
    {
        if (writeToMemory(oldFunc, jmp2func, sizeof(jmp2func)))
        {            
            *(void**)&jmp2func[1] = (char*)oldFunc + headerAsmAlign;
            memcpy(m_oldFuncBackup + headerAsmAlign, jmp2func, sizeof(jmp2func));

            m_isValid = true;
        }
    }
}

FuncHook::~FuncHook()
{
    if (isValid())
    {
        writeToMemory(m_oldFunc, m_oldFuncBackup, 7);
    }
}

bool FuncHook::isValid() const
{
    return m_isValid;
}