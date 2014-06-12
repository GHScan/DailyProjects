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

    void print(ostream &so) const {
        so << mData;
    }

    int getID() const { 
        return (unsigned char)mID;
    }

private:
    char mID, mData[1];
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
    ~SchemeSymbolPool() = default;

    SchemeSymbol* intern(const char *str, char id = 0) {
        int len = ::strlen(str);

        auto entry = mStrMap.get(str, len);
        if (entry == nullptr) {
            entry = mStrMap.insert(str, len, id);
        }

        return (SchemeSymbol*)entry;
    }

    bool isInterned(const char *str) const {
        return mStrMap.get(str, ::strlen(str)) != nullptr;
    }

private:
    StringMap<char> mStrMap;
};

#endif
