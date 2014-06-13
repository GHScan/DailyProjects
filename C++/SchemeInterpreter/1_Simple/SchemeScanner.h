#ifndef SCHEME_SCANNER_H
#define SCHEME_SCANNER_H

class SchemeSymbol;
class SchemeSymbolPool;

class SchemeScanner {
public:
    SchemeScanner(istream &si, SchemeSymbolPool *pool);

    const SchemeSymbol* nextToken();

    void putback(const SchemeSymbol *sym) {
        mPutbackStack.push_back(sym);
    }

private:
    istream &mStream;
    SchemeSymbolPool *mSymPool;
    vector<const SchemeSymbol*> mPutbackStack;
};

#endif
