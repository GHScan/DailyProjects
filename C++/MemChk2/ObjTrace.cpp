// #include "xysj_StableHeaders.h"
#include "stdafx.h"

#include <algorithm>
#include <string>
#include <fstream>
#include <map>

#include <windows.h>
#include <DbgHelp.h>

#include "Win32APIWrapper.h"
#include "ObjTrace.h"

class CodeSegmentQuery
{
public:
    bool isCodeAddress(void *p) const
    {
        VoidPtrVec::const_iterator iter = std::lower_bound(m_codeSegments.begin(), m_codeSegments.end(), p);
        if (iter != m_codeSegments.end())
        {
            size_t pos = std::distance(m_codeSegments.begin(), iter);
            return (pos & 1) == 1;
        }
        return false;
    }

    void resetCodeSegmentRange()
    {
        m_codeSegments.clear();

        Win32::ToolHelp32Iterator iter(Win32::ToolHelp32Iterator::HT_Module); 
        while (iter.hasMore())
        {
            MODULEENTRY32 module = iter.getNextModule();
            m_codeSegments.push_back(module.modBaseAddr + 0);
            m_codeSegments.push_back(module.modBaseAddr + module.modBaseSize);
        }
        std::sort(m_codeSegments.begin(), m_codeSegments.end());
    }

    static CodeSegmentQuery* getSingletonPtr()
    {
        static CodeSegmentQuery ls_ins;
        return &ls_ins;
    }

private:
    CodeSegmentQuery()
    {
        resetCodeSegmentRange();
    }

private:
    VoidPtrVec  m_codeSegments;
};  

void resetCodeSegRangeForCaptureStack()
{
    CodeSegmentQuery::getSingletonPtr()->resetCodeSegmentRange();
}

void captureStacks(VoidPtrVec& callStack, int skipDepth)
{
    void **_ebp = NULL;
    __asm mov _ebp, ebp;

    // windows的头64k为非法地址
    if ((int)_ebp <= 0x10000) return;

    while (!IsBadReadPtr(_ebp, 8) && CodeSegmentQuery::getSingletonPtr()->isCodeAddress(_ebp[1]))
    {
        if (skipDepth > 0)
        {
            --skipDepth;
        }
        else
        {
            callStack.push_back(_ebp[1]);
        }

        if ((void*)_ebp[0] <= _ebp) break;
        _ebp = (void**)_ebp[0];
    }

    /*STACKFRAME64 frame = {0};
    frame.AddrPC.Mode = AddrModeFlat;
    frame.AddrFrame.Mode = AddrModeFlat;
    frame.AddrStack.Mode = AddrModeFlat;
    __asm
    {
_eipHelper:
        mov eax, _eipHelper
        mov dword ptr [frame.AddrPC.Offset], eax
        mov dword ptr [frame.AddrFrame.Offset], ebp
        mov dword ptr [frame.AddrStack.Offset], esp
    }

    while (StackWalk64(
        IMAGE_FILE_MACHINE_I386, 
        GetCurrentProcess(), 
        GetCurrentThread(), 
        &frame, NULL, NULL, 
        SymFunctionTableAccess64, SymGetModuleBase64, NULL) && callStack.size() < 40)
    {
        if (frame.AddrReturn.Offset != 0)
        {
            callStack.push_back((void*)frame.AddrReturn.Offset);
        }
        else
        {
            break;
        }
    }*/
}

static inline Win32::DbgHelpWrapper* getDefaultDebugTool()
{
    static Win32::DbgHelpWrapper ls_helper;
    return &ls_helper;
}

void dumpStacks(std::string& s, const VoidPtrVec& callStack)
{
    Win32::DbgHelpWrapper *helper = getDefaultDebugTool();
    std::string file;
    std::string func;
    std::string module;
    int line;

    for (VoidPtrVec::const_iterator iter = callStack.begin();
        iter != callStack.end();
        ++iter)
    {
        void *p = *iter;
        if (p == NULL)
        {
            break;
        }

        char buf[256] = "";

        if (helper->addressToFileLine(p, file, line))
        {
            sprintf(buf, "%s(%d)", file.c_str(), line);
            s += buf;
            if (helper->addressToFuncName(p, func))
            {
                s += ": " + func + "\n";
            }
            else
            {
                s += "\n";
            }
        }
        else
        {
            if (!helper->addressToModuleName(p, module))
            {
                module.clear();
            }
            
            sprintf(buf, "没有符号的地址= 0x%x [%s]\n", (unsigned)p, module.empty() ? "" : module.c_str());
            s += buf;
        }
    }
}

ObjTrace::ObjTrace(void)
{
}

ObjTrace::~ObjTrace(void)
{
}

void ObjTrace::addObj(void *obj)
{
    VoidPtrVec& callStack = m_objMap[obj];
    callStack.clear();

    captureStacks(callStack);
}

void ObjTrace::removeObj(void *obj)
{
    m_objMap.erase(obj);
}

const VoidPtrVec* ObjTrace::getObjTrace(void *obj) const
{   
    ObjTraceMap::const_iterator iter = m_objMap.begin();
    if (iter != m_objMap.end())
    {
        return &iter->second;
    }
    return NULL;
}

ObjTrace::ObjTraceMap::const_iterator ObjTrace::getIterator() const
{
    return m_objMap.begin();
}

namespace 
{
    struct CallStack
    {
        CallStack(const VoidPtrVec& _stackFrame): stackFrame(_stackFrame){}

        VoidPtrVec  stackFrame;
        bool operator < (const CallStack& o) const
        {   
            if (!stackFrame.empty() && stackFrame.size() == o.stackFrame.size())
            {
                return memcmp(&stackFrame[0], &o.stackFrame[0], stackFrame.size() * sizeof(stackFrame[0])) < 0;
            }
            return stackFrame.size() < o.stackFrame.size();
        }
    };
}

void ObjTrace::dump(const char *fileName) const
{
    typedef std::map<CallStack, int>        CallStack2ObjCntMap;
    CallStack2ObjCntMap callCntMap;
    for (ObjTraceMap::const_iterator iter = m_objMap.begin();
        iter != m_objMap.end();
        ++iter)
    {
        ++callCntMap[iter->second];
    }    

    typedef std::multimap<int, CallStack>   ObjCnt2CallStackMap;
    ObjCnt2CallStackMap cntCallMap;
    for (CallStack2ObjCntMap::iterator iter = callCntMap.begin();
        iter != callCntMap.end();
        ++iter)
    {
        cntCallMap.insert(ObjCnt2CallStackMap::value_type(iter->second, iter->first));
    }

    char buf[128] = "";
    std::string s;
    s += "**********************************************\n";
    s += std::string("对象个数 :") + itoa((int)m_objMap.size(), buf, 10) + "\n";
    int allocPos = 0;
    for (ObjCnt2CallStackMap::reverse_iterator iter = cntCallMap.rbegin();
        iter != cntCallMap.rend();
        ++iter)
    {
        s += std::string("分配点 ") + itoa(allocPos++, buf, 10) + " , 对象 " + itoa(iter->first, buf, 10) + " 个\n";
        std::string ts;
        dumpStacks(ts, iter->second.stackFrame);
        s += ts;
        s += "\n";
    }       
    s += "**********************************************\n";
    s += "\n";

    if (fileName == NULL || fileName == std::string())
    {
        OutputDebugString(s.c_str());
    }
    else
    {
        std::ofstream(fileName) << s;
    }
}