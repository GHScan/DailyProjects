#ifndef PCH_H
#define PCH_H

// macros for llvm {
#define __STDC_LIMIT_MACROS 
#define __STDC_CONSTANT_MACROS 
#define __STDC_FORMAT_MACROS 
// }

#define CHECK_MEMORY_LEAKS

#ifdef _MSC_VER
#pragma warning(disable : 4996)

#ifdef CHECK_MEMORY_LEAKS
#define _CRTDBG_MAP_ALLOC
#include <stdlib.h>
#include <crtdbg.h>
#endif

#endif

#include <assert.h>
#include <string.h>

#include <algorithm>
#include <memory>
#include <exception>
#include <string>
#include <sstream>
#include <iostream>
#include <fstream>
#include <vector>
#include <map>
#include <unordered_map>
#include <set>

using namespace std;

#define _TO_STRING(s) #s
#define TO_STRING(s) _TO_STRING(s)
#define FILE_LINE __FILE__"(" TO_STRING(__LINE__) ")"

#ifdef NDEBUG
#define ASSERT1(b, msg) if (b); else throw Exception(string(FILE_LINE " : " #b "->") + msg)
#define ASSERT(b) ASSERT1(b, "")
#else
//#define ASSERT1(b, msg)  assert(b && msg)
//#define ASSERT(b) assert(b)
#define ASSERT1(b, msg) if (b); else throw Exception(string(FILE_LINE " : " #b "->") + msg)
#define ASSERT(b) ASSERT1(b, "")
#endif

class Exception:
    public exception {
public:
    Exception(const string& s): m_s(s) {
        assert(0);
    }
    ~Exception() throw(){}
    const char* what() const throw() {
        return m_s.c_str();
    }
    void addLine(const string& line) { m_s += "\n" + line; }
private:
    string m_s;
};

string format(const char *fmt, ...);

// headers for llvm {
#include <llvm/ADT/SmallVector.h>
#include <llvm/Analysis/Verifier.h>
#include <llvm/Assembly/PrintModulePass.h>
#include <llvm/BasicBlock.h>
#include <llvm/CallingConv.h>
#include <llvm/Constants.h>
#include <llvm/DerivedTypes.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/JIT.h>
#include <llvm/Function.h>
#include <llvm/GlobalVariable.h>
#include <llvm/IRBuilder.h>
#include <llvm/InlineAsm.h>
#include <llvm/Instructions.h>
#include <llvm/LLVMContext.h>
#include <llvm/Module.h>
#include <llvm/Pass.h>
#include <llvm/PassManager.h>
#include <llvm/Support/FormattedStream.h>
#include <llvm/Support/MathExtras.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Transforms/IPO.h>
#include <llvm/Transforms/IPO/PassManagerBuilder.h>
// }

#endif
