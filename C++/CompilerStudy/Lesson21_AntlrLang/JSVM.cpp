
#include "pch.h"
#include "JSVM.h"
#include "JSFunction.h"

StackFrame::StackFrame(JSFunction *_func, JSValue *_local):
    oldStackSize(0), func(_func), ip(0){
    localConstPtr[0] = _local;
    localConstPtr[1] = func != NULL ? &func->meta->constTable[0] : NULL;
}

void JSVM::accessGCObjects(vector<GCObject*> &objs) {
    for (auto &kv : m_globals) {
        if (auto obj = kv.first.gcAccess()) objs.push_back(obj);
        if (auto obj = kv.second.gcAccess()) objs.push_back(obj);
    }
    for (auto &value : m_values) {
        if (auto obj = value.gcAccess()) objs.push_back(obj);
    }
}

JSVM::JSVM() {
    m_frames.reserve(1024);
    m_values.reserve(1024 * 64);
    pushFrame(NULL, NULL);
}
JSVM::~JSVM() {
    popFrame();
}

void JSVM::pushFrame(JSFunction *func, JSValue *argsBegin) {
    m_frames.push_back(StackFrame(func, argsBegin));
    m_frames.back().oldStackSize = (int)m_values.size();
    m_values.resize((argsBegin - &m_values[0]) + func->meta->getLocalSpace());
}
void JSVM::popFrame() {
    m_values.resize(m_frames.back().oldStackSize);
    m_frames.pop_back();
}
