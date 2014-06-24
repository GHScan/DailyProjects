#ifndef SYMBOLTABLE_H
#define SYMBOLTABLE_H

class VarAddress {
public:
    static VarAddress createLocal(int varIndex) {
        return VarAddress(0, varIndex);
    }
    static VarAddress createFree(int envIndex, int varIndex) {
        ASSERT(envIndex > 0);
        return VarAddress(envIndex, varIndex);
    }
    static VarAddress createGlobal(int varIndex) {
        return VarAddress(-1, varIndex);
    }

    bool isLocal() const {
        return mEnvIndex == 0;
    }

    bool isFree() const {
        return mEnvIndex > 0;
    }

    bool isGlobal() const {
        return mEnvIndex == -1;
    }

    int getEnvIndex() const {
        ASSERT(isFree());
        return mEnvIndex;
    }

    int getVarIndex() const {
        return mVarIndex;
    }

    bool operator == (const VarAddress &o) const {
        return mEnvIndex == o.mEnvIndex && mVarIndex == o.mVarIndex;
    }

    bool operator != (const VarAddress &o) const {
        return !(*this == o);
    }

private:
    VarAddress(int envIndex, int varIndex):
        mEnvIndex(envIndex), mVarIndex(varIndex) {
    }

private:
    int mEnvIndex;
    int mVarIndex;
};


class SymbolTable {
public:
    SymbolTable(SymbolTable *parent): 
        mParent(parent), mNextIndex(0) {
    }

    SymbolTable(const SymbolTable&) = delete;
    SymbolTable& operator = (const SymbolTable&) = delete;

    void define(const string &name) {
        if (mParent == nullptr) {
            if (mName2Index.count(name) == 0) {
                mName2Index[name] = mNextIndex++;
            }
        } else {
            ASSERT(mName2Index.count(name) == 0);
            mName2Index[name] = mNextIndex++;
        }
    }

    VarAddress lookup(const string &name) {
        int envIndex = 0;
        SymbolTable *table = this;
        while (table->mParent != nullptr && table->mName2Index.count(name) == 0) {
            ++envIndex;
            table = table->mParent;
        }

        if (table->mParent == nullptr) {
            table->define(name);
            return VarAddress::createGlobal(table->mName2Index[name]);
        } else {
            return envIndex == 0 ? 
                VarAddress::createLocal(table->mName2Index[name]) : 
                VarAddress::createFree(envIndex, table->mName2Index[name]);
        }
    }

    vector<string> getSymbols() const {
        vector<string> symbols(getSymbolCount());
        for (auto &kv : mName2Index) {
            symbols[kv.second] = kv.first;
        }
        return symbols;
    }

    int getSymbolCount() const {
        return mNextIndex;
    }

    string getSymbolByIndex(int index) const {
        for (auto &kv : mName2Index) {
            if (kv.second == index) return kv.first;
        }
        return "@unknown";
    }

private:
    SymbolTable *mParent;
    int mNextIndex;
    unordered_map<string, int> mName2Index;
};

#endif
