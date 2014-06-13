#ifndef SCHEME_SYMBOL_H
#define SCHEME_SYMBOL_H

#include "StringMap.h"

class SchemeSymbol {
public:
    enum {
        ID_None,
        ID_Number,
        ID_String,
        ID_LParenthese,
        ID_RParenthese,
        ID_Dot,
        ID_If,
        ID_Begin,
        ID_Lambda,
        ID_Quote,
        ID_Quasiquote,
        ID_Unquote,
        ID_UnquoteSlicing,
        ID_Define,
        ID_Set,
    };

public:
    SchemeSymbol() = delete;
    SchemeSymbol(const SchemeSymbol &o) = delete;
    SchemeSymbol& operator = (const SchemeSymbol &o) = delete;
    ~SchemeSymbol() = delete;

    bool operator == (const SchemeSymbol &o) const {
        return this == &o;
    }
    bool operator != (const SchemeSymbol& o) const {
        return !(*this == o);
    }

    int getID() const { 
        return (unsigned char)mID;
    }

    const char *c_str() const {
        return mData;
    }

private:
    char mID, mData[1];
};

class SchemeSymbolPool {
public:
    SchemeSymbolPool() {
        intern("(", SchemeSymbol::ID_LParenthese);
        intern(")", SchemeSymbol::ID_RParenthese);
        intern(".", SchemeSymbol::ID_Dot);
        intern("if", SchemeSymbol::ID_If);
        intern("begin", SchemeSymbol::ID_Begin);
        intern("lambda", SchemeSymbol::ID_Lambda);
        intern("quote", SchemeSymbol::ID_Quote);
        intern("quasiquote", SchemeSymbol::ID_Quasiquote);
        intern("unquote", SchemeSymbol::ID_Unquote);
        intern("unquote-slicing", SchemeSymbol::ID_UnquoteSlicing);
        intern("define", SchemeSymbol::ID_Define);
        intern("set!", SchemeSymbol::ID_Set);
    }

    SchemeSymbolPool(const SchemeSymbolPool &o) = delete;
    SchemeSymbolPool& operator = (const SchemeSymbolPool &o) = delete;
    ~SchemeSymbolPool() = default;

    const SchemeSymbol* intern(const char *str, char id = 0) {
        int len = ::strlen(str);

        auto entry = mStrMap.get(str, len);
        if (entry == nullptr) {
            entry = mStrMap.insert(str, len, id);
        }

        return reinterpret_cast<SchemeSymbol*>(entry);
    }

    bool isInterned(const char *str) const {
        return mStrMap.get(str, ::strlen(str)) != nullptr;
    }

private:
    StringMap<char> mStrMap;
};

#endif
