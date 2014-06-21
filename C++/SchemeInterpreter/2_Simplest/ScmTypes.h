#ifndef SCMTYPES_H
#define SCMTYPES_H

#include "ScmObject.h"
#include <integer.h>

struct ScmSymbol: public ScmObject {
    static const ScmObjType TYPE = SOT_Symbol;

    const string *str;

    uint32_t _getHashCode() const {
        return hash<const ScmSymbol*>()(this);
    }

    bool _equal(const ScmSymbol *o) const {
        return false;
    }

    void _writeToStream(ostream &so) const {
        so << escapeString(*str, "");
    }

private:
    friend class ScmObjectManager;

    ScmSymbol(const string *_str): 
        ScmObject(TYPE), str(_str) {
    }
};

struct ScmInt: public ScmObject {
    static const ScmObjType TYPE = SOT_Int;

    typedef int Int;

    Int number;

    uint32_t _getHashCode() const {
        return hash<int>()(number);
    }

    bool _equal(const ScmInt *o) const {
        return number == o->number;
    }

    void _writeToStream(ostream &so) const {
        so << number;
    }

private:
    friend class ScmObjectManager;

    ScmInt(Int _number): 
        ScmObject(TYPE), number(_number) {
    }
};

struct ScmBigInt: public ScmObject {
    static const ScmObjType TYPE = SOT_BigInt;

    typedef CryptoPP::Integer BigInt;
    typedef CryptoPP::word Word;
    static const int WORD_BIT_COUNT = sizeof(Word) * 8;

    const BigInt number;

    uint32_t _getHashCode() const {
        if (mHashCode == 0) {
            hash<Word> hash;

            mHashCode = hash(number.IsNegative());
            for (int count = number.WordCount(), i = 0; i < count; ++i) {
                mHashCode = hashMerge(mHashCode, hash((Word)number.GetBits(i * WORD_BIT_COUNT, WORD_BIT_COUNT)));
            }
        }
        return mHashCode;
    }

    bool _equal(const ScmBigInt *o) const {
        if (mHashCode && o->mHashCode && mHashCode != o->mHashCode) {
            return false;
        }
        return number == o->number;
    }

    void _writeToStream(ostream &so) const {
        so << number;
    }

    double toDouble() const;

private:
    mutable uint32_t mHashCode;

private:
    friend class ScmObjectManager;

    ScmBigInt(const BigInt &_number): 
        ScmObject(TYPE), number(_number), mHashCode(0) {
    }
};

struct ScmDouble: public ScmObject {
    static const ScmObjType TYPE = SOT_Double;

    double number;

    uint32_t _getHashCode() const {
        return hash<double>()(number);
    }

    bool _equal(const ScmDouble *o) const {
        return number == o->number;
    }

    void _writeToStream(ostream &so) const {
        so << number;
    }

    ScmBigInt::BigInt toBigInt() const;
private:
    friend class ScmObjectManager;

    ScmDouble(double _number): 
        ScmObject(TYPE), number(_number) {
    }
};

struct ScmString: public ScmObject {
    static const ScmObjType TYPE = SOT_String;

    string str;

    uint32_t _getHashCode() const {
        return hash<string>()(str);
    }

    bool _equal(const ScmString *o) const {
        return str == o->str;
    }

    void _writeToStream(ostream &so) const {
        so << '"' << escapeString(str, "\"") << '"';
    }

private:
    friend class ScmObjectManager;

    ScmString(const string &_str): 
        ScmObject(TYPE), str(_str) {
    }
};

struct ScmPair: public ScmObject {
    static const ScmObjType TYPE = SOT_Pair;

    ScmObject *car;
    ScmObject *cdr;

    void markChildren(queue<ScmObject*>* objs) {
        if (car->mark()) objs->push(car);
        if (cdr->mark()) objs->push(cdr);
    }

    uint32_t _getHashCode() const {
        return hashMerge(car->getHashCode(), cdr->getHashCode());
    }

    bool _equal(const ScmPair *o) const {
        return car->equal(o->car) && cdr->equal(o->cdr);
    }

    void _writeToStream(ostream &so) const;

private:
    friend class ScmObjectManager;

    ScmPair(ScmObject *_car, ScmObject *_cdr): 
        ScmObject(TYPE), car(_car), cdr(_cdr) {
    }
};

struct ScmVector: public ScmObject {
    static const ScmObjType TYPE = SOT_Vector;

    const vector<ScmObject*>& getImmutableVec() const {
        return mVec;
    }

    vector<ScmObject*>& getMutableVec() {
        mHashCode = 0;
        return mVec;
    }

    void markChildren(queue<ScmObject*>* objs) {
        for (auto obj : mVec) {
            if (obj->mark()) objs->push(obj);
        }
    }

    uint32_t _getHashCode() const {
        if (mHashCode == 0) {
            mHashCode = hashMerge(mHashCode, 0);
            for (auto v : mVec) {
                mHashCode = hashMerge(mHashCode, v->getHashCode());
            }
        }
        return mHashCode;
    }

    bool _equal(const ScmVector *o) const {
        if (mVec.size() != o->mVec.size()) return false;
        if (mHashCode && o->mHashCode && mHashCode != o->mHashCode) return false;

        for (int i = 0; i < (int)mVec.size(); ++i) {
            if (!mVec[i]->equal(o->mVec[i])) return false;
        }

        return true;
    }

    void _writeToStream(ostream &so) const {
        so << '[';
        for (int i = 0; i < (int)mVec.size(); ++i) {
            if (i > 0) so << ',';
            mVec[i]->writeToStream(so);
        }
        so << ']';
    }

    ScmObject** alloc() {
        mHashCode = 0;
        mVec.push_back(nullptr);
        return &mVec.back();
    }

    void popBack() {
        mHashCode = 0;
        mVec.pop_back();
    }

    bool size() const {
        return mVec.size();
    }
private:
    vector<ScmObject*> mVec;
    mutable uint32_t mHashCode;

private:
    friend class ScmObjectManager;

    ScmVector(): 
        ScmObject(TYPE), mHashCode(0) {
    }
};

struct ScmDictionary: public ScmObject {
    static const ScmObjType TYPE = SOT_Dictionary;

    const unordered_map<ScmObject*, ScmObject*>& getImmutableDict() const {
        return mDict;
    }

    unordered_map<ScmObject*, ScmObject*>& getImmutableDict() {
        return mDict;
    }

    void markChildren(queue<ScmObject*>* objs) {
        for (auto &kv : mDict) {
            if (kv.first->mark()) objs->push(kv.first);
            if (kv.second->mark()) objs->push(kv.second);
        }
    }

    uint32_t _getHashCode() const {
        if (mHashCode == 0) {
            mHashCode = hashMerge(mHashCode, 0);
            for (auto &kv : mDict) {
                mHashCode = hashMerge(mHashCode, kv.first->getHashCode());
                mHashCode = hashMerge(mHashCode, kv.second->getHashCode());
            }
        }
        return mHashCode;
    }

    bool _equal(const ScmDictionary *o) const {
        if (mDict.size() != o->mDict.size()) return false;
        if (mHashCode && o->mHashCode && mHashCode != o->mHashCode) return false;

        for (auto iter1 = mDict.begin(), iter2 = o->mDict.begin(); iter1 != mDict.end(); ++iter1, ++iter2) {
            if (!iter1->first->equal(iter2->first)) return false;
            if (!iter1->second->equal(iter2->second)) return false;
        }

        return true;
    }

    void _writeToStream(ostream &so) const {
        so << format("{dict:%p}", this);
    }

private:
    unordered_map<ScmObject*, ScmObject*> mDict;
    mutable uint32_t mHashCode;

private:
    friend class ScmObjectManager;

    ScmDictionary(): 
        ScmObject(TYPE) {
    }
};

struct ScmEnv: public ScmObject {
    static const ScmObjType TYPE = SOT_Env;

    ScmEnv *prevEnv;
    ScmDictionary* dict;

    void markChildren(queue<ScmObject*>* objs) {
        if (prevEnv->mark()) objs->push(prevEnv);
        if (dict->mark()) objs->push(dict);
    }

    uint32_t _getHashCode() const {
        return hashMerge(prevEnv->getHashCode(), dict->getHashCode());
    }

    bool _equal(const ScmEnv *o) const {
        return prevEnv->equal(o->prevEnv) && dict->equal(o->dict);
    }

    void _writeToStream(ostream &so) const {
        so << format("{env:%p}", this);
    }

private:
    friend class ScmObjectManager;

    ScmEnv(ScmEnv *_prevEnv, ScmDictionary* _dict): 
        ScmObject(TYPE), prevEnv(_prevEnv), dict(_dict) {
    }
};

struct ScmScriptFunction: public ScmObject {
    static const ScmObjType TYPE = SOT_ScriptFunction;

    ScmVector *formals;
    ScmObject *body;
    ScmEnv *env;

    void markChildren(queue<ScmObject*>* objs) {
        if (formals->mark()) objs->push(formals);
        if (body->mark()) objs->push(body);
        if (env->mark()) objs->push(env);
    }

    uint32_t _getHashCode() const {
        uint32_t h = hashMerge(formals->getHashCode(), body->getHashCode());
        h = hashMerge(h, env->getHashCode());
        return h;
    }

    bool _equal(const ScmScriptFunction *o) const {
        return formals->equal(o->formals) && body->equal(o->body) && env->equal(o->env);
    }

    void _writeToStream(ostream &so) const {
        so << format("{scriptFunction:%p}", this);
    }

private:
    friend class ScmObjectManager;

    ScmScriptFunction(ScmVector *_formals, ScmObject *_body) :
        ScmObject(TYPE), formals(_formals), body(_body) {
    }
};

struct ScmCFunction: public ScmObject {
    static const ScmObjType TYPE = SOT_CFunction;

    CFunction func;

    uint32_t _getHashCode() const {
        return hash<CFunction>()(func);
    }

    bool _equal(const ScmCFunction *o) const {
        return func == o->func;
    }

    void _writeToStream(ostream &so) const {
        so << format("{cfunction:%p}", this);
    }

private:
    friend class ScmObjectManager;

    ScmCFunction(CFunction _func): 
        ScmObject(TYPE), func(_func) {
    }
};

#endif
