#ifndef SCHEME_PORT_H
#define SCHEME_PORT_H

#include "SchemeDynamicObject.h"
#include "SchemeRef.h"

class SchemeMemoryManager;
class SchemeSymbolPool;
class SchemeString;

class SchemeInputPort: public SchemeDynamicObject {
public:
    static SchemeInputPort* openFile(SchemeMemoryManager *mgr, const char *fname);
    static SchemeInputPort* openTextFile(SchemeMemoryManager *mgr, const char *fname);
    static SchemeInputPort* attachStream(SchemeMemoryManager *mgr, istream *si, bool isOwner);

    SchemeRef readString(SchemeMemoryManager* mgr, int len);
    SchemeRef readSExpression(SchemeMemoryManager* mgr, SchemeSymbolPool *pool);

    virtual bool equal(const SchemeDynamicObject &o) const override {
        assert(this != &o);
        return false;
    }

    static void installEOF(SchemeRef p) {
        EOF_OF_PORT = p;
    }

    static SchemeRef EOF_OF_PORT;

private:
    shared_ptr<istream> mStream;
};

class SchemeOutputPort: public SchemeDynamicObject {
public:
    static SchemeOutputPort* openFile(SchemeMemoryManager *mgr, const char *fname);
    static SchemeOutputPort* openTextFile(SchemeMemoryManager *mgr, const char *fname);
    static SchemeOutputPort* attachStream(SchemeMemoryManager* mgr, ostream *so, bool isOwner);

    void writeString(const SchemeRef &s);
    void writeSExpression(const SchemeRef &exp);
    void writeln();

    virtual bool equal(const SchemeDynamicObject &o) const override {
        assert(this != &o);
        return false;
    }

private:
    shared_ptr<ostream> mStream;
};

#endif
