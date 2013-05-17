
#include "pch.h"
#include "JSVM.h"
#include "JSFunction.h"

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
    // TODO
    m_frames.reserve(1024);
    m_values.reserve(1024 * 64);
    m_values.push_back(JSValue::NIL);
}

void JSVM::pushFrame(JSFunction *func, int retStackIdx) {
    JSValue *ret = NULL;
    if (m_frames.empty()) ret = &m_values[0];
    else {
        auto &lastFrame = m_frames.back();
        ret = lastFrame.stack + retStackIdx;
    }
    StackFrame frame = {&m_values[0] + m_values.size(), ret, func};
    m_frames.push_back(frame);
    m_values.resize(m_values.size() + func->meta->getLocalSpace());
}
void JSVM::popFrame() {
    m_frames.pop_back();
    if (!m_frames.empty()) {
        auto& lastFrame = m_frames.back();
        int newSize = (lastFrame.stack - &m_values[0]) + lastFrame.func->meta->getLocalSpace();
        m_values.resize(newSize);
    }
}
