#include "pch.h"
#include "SchemeParser.h"
#include "SchemeStaticObject.h"
#include "SchemeDynamicObject.h"
#include "SchemeScanner.h"
#include "SchemeSymbol.h"
#include "SchemeList.h"

SchemeRef SchemeParser::parse(SchemeMemoryManager *mgr, SchemeScanner *scanner) {
    auto sym = scanner->nextToken();
    switch (sym->getID()) {
        case SchemeSymbol::ID_Number: {
            const char *s = sym->c_str();
            if (strchr(s, '.')) {
                return SchemeFloat::create(mgr, s);
            } else {
                long l = strtol(s, nullptr, 10);
                if (errno == ERANGE || l < SchemeRef::MIN_INTEGER || l > SchemeRef::MAX_INTEGER) {
                    return SchemeBigInteger::create(mgr, s);
                } else {
                    return SchemeRef(PtrValue(l));
                }
            }
          }
        case SchemeSymbol::ID_String:
            return SchemeString::create(mgr, sym->c_str());
        case SchemeSymbol::ID_LParenthese: {
            if ((sym = scanner->nextToken())->getID() == SchemeSymbol::ID_RParenthese) {
                return SchemeList::EMPTY;
            } 
            scanner->putback(sym);

            SchemePair *head, *tail;
            head = tail = SchemePair::create(mgr);
            head->setCar(parse(mgr, scanner));

            for (;;) {
                sym = scanner->nextToken();
                if (sym->getID() == SchemeSymbol::ID_RParenthese) {
                    tail->setCdr(SchemeList::EMPTY);
                    break;
                } else if (sym->getID() == SchemeSymbol::ID_Dot) {
                    tail->setCdr(parse(mgr, scanner));

                    sym = scanner->nextToken();
                    assert(sym ->getID() == SchemeSymbol::ID_RParenthese);
                    break;
                } else {
                    scanner->putback(sym);

                    SchemePair *newPair = SchemePair::create(mgr);
                    tail->setCdr(newPair);
                    tail = newPair;
                    tail->setCar(parse(mgr, scanner));
                }
            }

            return head;
           }
        default:
            return sym;
    }
}
