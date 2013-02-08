
#ifndef TYPE_SYSTEM_H
#define TYPE_SYSTEM_H

struct IType
{
    virtual ~IType(){}
    virtual int getSize() = 0;
};

struct SimpleType:
    public IType
{
    int width;
    explicit SimpleType(int w): width(w){}
    virtual int getSize() { return width; }
};
struct StructType:
    public IType
{
    vector<IType*> fields;
    virtual int getSize() 
    { 
        int r = 0;
        for (auto t : fields) r += t->getSize();
        return r;
    }
};
struct FunctionType:
    public IType
{
    IType *retT;
    vector<IType*> argsT;
    bool isVarLengOfArg;
    FunctionType(IType *_retT, const vector<IType*>& _argsT): retT(_retT), argsT(_argsT), isVarLengOfArg(false){}
    virtual int getSize() { return sizeof(void*); }
};
struct PointerType:
    public IType
{
    IType *refType;
    explicit PointerType(IType *_refType): refType(_refType){}
    virtual int getSize() { return sizeof(void*) ;}
};
struct ArrayType:
    public IType
{
    IType *elemType;
    int length;
    explicit ArrayType(IType *_elemType, int len): elemType(_elemType), length(len){}
    virtual int getSize() { return elemType->getSize() * length; }
};

class TypeSystem
{
public:
    static TypeSystem* instance()
    {
        static TypeSystem s_ins;
        return &s_ins;
    }
    TypeSystem();
    ~TypeSystem();

    IType* addType(const string& name, IType *type);
    IType* getType(const string& name);
    IType* getPointer(IType *type);
    IType* getArray(IType *elemType, int n);
    IType* getFunc(IType* ret, const vector<IType*>& args);
private:
    map<string, IType*> m_name2Type;
    map<IType*, IType*> m_type2Pointer;
    map<IType*, map<int, IType*> > m_type2Array;
    map<string, IType*> m_type2Func;
};

#define TYPE(s) TypeSystem::instance()->getType(s)

#endif
