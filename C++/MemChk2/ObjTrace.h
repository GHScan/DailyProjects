#pragma once

#include <vector>
#include <map>
#include <string>

typedef std::vector<void*>  VoidPtrVec;

void captureStacks(VoidPtrVec& callStack, int skipDepth = 0);
void resetCodeSegRangeForCaptureStack();

void dumpStacks(std::string& s, const VoidPtrVec& callStack);

class ObjTrace
{
public:
    typedef std::map<void*, VoidPtrVec> ObjTraceMap;

public:
    ObjTrace(void);
    ~ObjTrace(void);

    void addObj(void *obj);
    void removeObj(void *obj);
    const VoidPtrVec* getObjTrace(void *obj) const;
    ObjTraceMap::const_iterator getIterator() const;
    void dump(const char *fileName = NULL) const;

private:
    ObjTraceMap     m_objMap;
};
