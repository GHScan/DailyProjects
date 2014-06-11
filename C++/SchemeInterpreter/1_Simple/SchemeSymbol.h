#ifndef SCHEME_SYMBOL_H
#define SCHEME_SYMBOL_H

#include "StringMap.h"

class SchemeSymbol {
public:
    SchemeSymbol() = delete;
    SchemeSymbol(const SchemeSymbol &o) = delete;
    SchemeSymbol& operator = (const SchemeSymbol &o) = delete;
    ~SchemeSymbol() = delete;

    bool operator == (const SchemeSymbol &o) const {
        return this == &o;
    }

    void print(ostream &so) {
        so << data;
    }

private:
    char data[1];
};

inline ostream& operator << (ostream& so, SchemeSymbol &sym) {
    sym.print(so);
    return so;
}

class SchemeSymbolPool {
public:
    SchemeSymbolPool() = default;
    SchemeSymbolPool(const SchemeSymbolPool &o) = delete;
    SchemeSymbolPool& operator = (const SchemeSymbolPool &o) = delete;

    static SchemeSymbolPool* instance() {
        static SchemeSymbolPool s_ins;
        return &s_ins;
    }

    SchemeSymbol* intern(const char *str) {
        int len = ::strlen(str);

        auto entry = mStrMap.get(str, len);
        if (entry == nullptr) {
            entry = mStrMap.insert(str, len, 0);
        }

        return (SchemeSymbol*)entry->key;
    }

private:
    StringMap<char> mStrMap;
};

#endif
