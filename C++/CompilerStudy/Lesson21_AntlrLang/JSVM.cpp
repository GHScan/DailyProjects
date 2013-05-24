
#include "pch.h"
#include "JSVM.h"
#include "JSFunction.h"
#include "JSString.h"
#include "GCObject.h"

StackFrame::StackFrame(JSFunction *_func, JSValue *_local):
    oldStackSize(0), func(_func), ip(0){
    localConstPtr[0] = _local;
    localConstPtr[1] = func != NULL ? &func->meta->constTable[0] : NULL;
}

JSVM* JSVM::s_ins;
void JSVM::accessGCObjects(vector<GCObject*> &objs) {
    for (auto &kv : m_globals) {
        if (auto obj = kv.first.gcAccess()) objs.push_back(obj);
        if (auto obj = kv.second.gcAccess()) objs.push_back(obj);
    }
    for (auto &value : m_values) {
        if (auto obj = value.gcAccess()) objs.push_back(obj);
    }
    for (auto &meta : m_metas) {
        meta->accessGCObjects(objs);
    }
    for (auto &frame : m_frames) {
        if (frame.func == NULL) continue;
        if (auto obj = frame.func->gcAccess()) objs.push_back(obj);
    }
}

JSVM::JSVM() {
    GCObjectManager::createInstance();
    JSStringManager::createInstance();

    m_frames.reserve(1024);
    m_values.reserve(1024 * 64);
    m_values.push_back(JSValue::NIL);
    pushFrame(NULL, &m_values[0]);
}
JSVM::~JSVM() {
    popFrame();
    ASSERT(m_values.size() == 1 && m_frames.empty());
    m_values.clear();
    m_globals.clear();
    m_metas.clear();
    GCObjectManager::instance()->performFullGC();

    JSStringManager::destroyInstance();
    GCObjectManager::destroyInstance();
}

void JSVM::pushFrame(JSFunction *func, JSValue *argsBegin) {
    m_frames.push_back(StackFrame(func, argsBegin));
    m_frames.back().oldStackSize = (int)m_values.size();
    if (func != NULL) {
        m_values.resize((argsBegin - &m_values[0]) + func->meta->getLocalSpace());
    }
}
void JSVM::popFrame() {
    m_values.resize(m_frames.back().oldStackSize);
    m_frames.pop_back();
}
