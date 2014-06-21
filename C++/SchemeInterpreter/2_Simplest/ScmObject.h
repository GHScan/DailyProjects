#ifndef SCMOBJECT_H
#define SCMOBJECT_H

class ScmObjectManager;

enum ScmObjType {
    SOT_Symbol,
    SOT_Int,
    SOT_BigInt,
    SOT_Double,
    SOT_String,
    SOT_Pair,
    SOT_Vector,
    SOT_Dictionary,
    SOT_Env,
    SOT_ScriptFunction,
    SOT_CFunction,
};

struct ScmObject {
    uint32_t marked : 1;
    uint32_t type : 10;
    ScmObject *next;

    ScmObject(const ScmObject&) = delete;
    ScmObject& operator = (const ScmObject&) = delete;

    template<typename DerivedType>
    DerivedType* staticCast() {
        ASSERT(type == DerivedType::TYPE);
        return static_cast<DerivedType*>(this);
    }

    template<typename DerivedType>
    const DerivedType* dynamicCast() const {
        return type == DerivedType::TYPE ? static_cast<const DerivedType*>(this) : nullptr;
    }

    template<typename DerivedType>
    DerivedType* dynamicCast() {
        return type == DerivedType::TYPE ? static_cast<DerivedType*>(this) : nullptr;
    }

    bool isMarked() const {
        return marked == 1;
    }

    void unmark() {
        marked = 0;
    }

    bool mark() {
        bool b = marked == 0;
        marked = 1;
        return b;
    }

    int getHashCode() const;

    bool equal(const ScmObject *o) const {
        if (this == o) return true;
        if (type != o->type) return false;
        return equalDispatch(o);
    }

    void writeToStream(ostream &so) const;

    static ScmObject *EMPTY;
private:
    bool equalDispatch(const ScmObject *o) const;

protected:
    ScmObject(int _type) {
        marked = 0;
        type = _type;
    }
};

typedef void (*CFunction)(ScmObjectManager *mgr, ScmObject **ret, ScmObject **argEnd);

#endif
