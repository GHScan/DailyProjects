#ifndef SSYMBOL_H
#define SSYMBOL_H

#include "StringMap.h"

class SSymbol: public StringMap<uint8_t>::Entry {
public:
    enum SymbolID {
        ID_Default,
        ID_Quote,
        ID_If,
        ID_Begin,
        ID_Lambda,
        ID_Define,
        ID_Set,
    };

    int getID() const {
        return value;
    }

    const char *c_str() const {
        return key;
    }

    bool operator == (const SSymbol &o) const {
        return this == &o;
    }

    bool operator != (const SSymbol &o) const {
        return this != &o;
    }

    SSymbol() = delete;
    SSymbol(const SSymbol&) = delete;
    SSymbol& operator = (const SSymbol&) = delete;
};

class SSymbolManager {
public:
    SSymbolManager();

    SSymbol* getSymbol(const char *str, int id = SSymbol::ID_Default) {
        int len = strlen(str);

        auto entry = mMap.get(str, len);
        if (entry == nullptr) {
            entry = mMap.insert(str, len, (uint8_t)id);
        }

        return static_cast<SSymbol*>(entry);
    }

    SSymbolManager(const SSymbolManager&);
    SSymbolManager& operator = (const SSymbolManager&);

private:
    StringMap<uint8_t> mMap;
};

#endif
