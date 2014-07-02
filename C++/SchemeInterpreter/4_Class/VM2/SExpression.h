#ifndef SEXPRESSION_H
#define SEXPRESSION_H

#include "Atom.h"

struct SExpressionListNode;

class SExpression {
public:
    static SExpression createInt(Atom *atom) {
        SExpression r;
        r.mType = T_Int;
        r.mAtom = atom;
        return r;
    }

    static SExpression createFloat(Atom *atom) {
        SExpression r;
        r.mType = T_Float;
        r.mAtom = atom;
        return r;
    }

    static SExpression createString(Atom *atom) {
        SExpression r;
        r.mType = T_String;
        r.mAtom = atom;
        return r;
    }

    static SExpression createSymbol(Atom *atom) {
        SExpression r;
        r.mType = T_Symbol;
        r.mAtom = atom;
        return r;
    }

    static SExpression createList(SExpressionListNode *node) {
        SExpression r;
        r.mType = T_List;
        r.mNode = node;
        return r;
    }

    Atom* getInt() {
        return mType == T_Int ? mAtom : nullptr;
    }

    Atom* getFloat() {
        return mType == T_Float ? mAtom : nullptr;
    }

    Atom* getString() {
        return mType == T_String ? mAtom : nullptr;
    }

    Atom* getSymbol() {
        return mType == T_Symbol ? mAtom : nullptr;
    }

    SExpressionListNode* getNode() { 
        return mType == T_List ? mNode : nullptr;
    }

private:
    union {
        Atom *mAtom;
        SExpressionListNode *mNode;
    };
    enum {
        T_Int,
        T_Float,
        T_String,
        T_Symbol,
        T_List,
    } mType;
};

struct SExpressionListNode {
    SExpression value;
    SExpressionListNode *next;

    SExpression ref(int i) {
        ASSERT(this != nullptr);
        ASSERT(i >= 0);
        return i == 0 ? value : next->ref(i - 1);
    }
};

#endif
