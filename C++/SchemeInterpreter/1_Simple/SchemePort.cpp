#include "pch.h"

#include <fstream>

#include "SchemePort.h"
#include "SchemeMemoryManager.h"
#include "SchemeStaticObject.h"
#include "SchemePrinter.h"
#include "SchemeScanner.h"
#include "SchemeParser.h"

SchemeRef SchemeInputPort::EOF_OF_PORT;

SchemeInputPort* SchemeInputPort::openFile(SchemeMemoryManager *mgr, const char *fname) {
    SchemeInputPort *port = new SchemeInputPort();

    port->mStream = shared_ptr<istream>(new ifstream(fname, ios::binary));
    mgr->addDynamicObject(port);

    return port;
}

SchemeInputPort* SchemeInputPort::openTextFile(SchemeMemoryManager *mgr, const char *fname) {
    SchemeInputPort *port = new SchemeInputPort();

    port->mStream = shared_ptr<istream>(new ifstream(fname));
    mgr->addDynamicObject(port);

    return port;
}

SchemeInputPort* SchemeInputPort::attachStream(SchemeMemoryManager *mgr, istream *si, bool isOwner) {
    SchemeInputPort *port = new SchemeInputPort();

    if (isOwner) {
        port->mStream = shared_ptr<istream>(si);
    } else {
        port->mStream = shared_ptr<istream>(si, [](istream *p){});
    }
    mgr->addDynamicObject(port);

    return port;
}


SchemeRef SchemeInputPort::readString(SchemeMemoryManager* mgr, int len) {
    if (!*mStream) return EOF_OF_PORT;

    SchemeString *s = SchemeString::create(mgr, "");

    s->str.resize(len);
    mStream->read(&s->str[0], len);
    s->str.resize(mStream->gcount());

    return s;
}

SchemeRef SchemeInputPort::readSExpression(SchemeMemoryManager* mgr, SchemeSymbolPool *pool) {
    if (!*mStream) return EOF_OF_PORT;

    SchemeScanner scanner(*mStream, pool);
    return SchemeParser::parse(mgr, &scanner);
}

SchemeOutputPort* SchemeOutputPort::openFile(SchemeMemoryManager *mgr, const char *fname) {
    SchemeOutputPort *port = new SchemeOutputPort();

    port->mStream = shared_ptr<ostream>(new ofstream(fname, ios::binary));
    mgr->addDynamicObject(port);

    return port;
}

SchemeOutputPort* SchemeOutputPort::openTextFile(SchemeMemoryManager *mgr, const char *fname) {
    SchemeOutputPort *port = new SchemeOutputPort();

    port->mStream = shared_ptr<ostream>(new ofstream(fname));
    mgr->addDynamicObject(port);

    return port;
}

SchemeOutputPort* SchemeOutputPort::attachStream(SchemeMemoryManager* mgr, ostream *so, bool isOwner) {
    SchemeOutputPort *port = new SchemeOutputPort();

    if (isOwner) {
        port->mStream = shared_ptr<ostream>(so);
    } else {
        port->mStream = shared_ptr<ostream>(so, [](ostream *p){});
    }
    mgr->addDynamicObject(port);

    return port;
}

void SchemeOutputPort::writeString(const SchemeRef &s) {
    *mStream << dynamic_cast<const SchemeString*>(s.getDynamicObject())->str;
}

void SchemeOutputPort::writeSExpression(const SchemeRef &exp) {
    SchemePrinter::printSExpression(*mStream, exp);
}

void SchemeOutputPort::writeln() {
    *mStream << endl;
}
