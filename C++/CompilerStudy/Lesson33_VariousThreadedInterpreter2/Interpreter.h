#ifndef INTERPRETER_H
#define INTERPRETER_H
//==============================
#define CodeSize 4
#define LocalIdxSize 4
#define JmpOffSize 4
#define EvalStackSize 32
#define LocalStackSize 64

template<int size, bool isSigned> struct FixedSizeType;
template<> struct FixedSizeType<1, true> { typedef char Type; };
template<> struct FixedSizeType<1, false> { typedef unsigned char Type; };
template<> struct FixedSizeType<2, true> { typedef short Type; };
template<> struct FixedSizeType<2, false> { typedef unsigned short Type; };
template<> struct FixedSizeType<4, true> { typedef int Type; };
template<> struct FixedSizeType<4, false> { typedef unsigned int Type; };
template<> struct FixedSizeType<8, true> { typedef long long Type; };
template<> struct FixedSizeType<8, false> { typedef unsigned long long Type; };

typedef FixedSizeType<CodeSize, false>::Type CodeType;
typedef FixedSizeType<LocalIdxSize, false>::Type LocalIdxType;
typedef FixedSizeType<JmpOffSize, true>::Type JmpOffType;

//==============================
class InstructionMeta {
public:
    CodeType code;
    string name;
    int size;

    InstructionMeta(int _code, const string& _name) : code(_code), name(_name), size(CodeSize) {}
    virtual ~InstructionMeta(){}
    void fromStream(istream& si, void *ins) {
        //string _name;
        //si >> _name;
        //assert(_name == name);
        *(CodeType*)ins = code;
        fieldsFromStream(si, (char*)ins + CodeSize);
    }
    void toStream(ostream& so, const void *ins) {
        so << name << ' ';
        fieldsToStream(so, (char*)ins + CodeSize);
    }
protected:
    virtual void fieldsFromStream(istream &si, void *ins) {}
    virtual void fieldsToStream(ostream &so, const void *ins) {}
    template<typename T>
    void* readField(istream &si, void *ins) {
        si >> *(T*)ins;
        assert(si);
        return (char*)ins + sizeof(T);
    }
    template<typename T>
    void* writeField(ostream &so, const void *ins) {
        so << *(T*)ins << ' ';
        return (char*)ins + sizeof(T);
    }
};
template<typename T1>
struct InstructionMeta1: public InstructionMeta {
    InstructionMeta1(int code, const string &name): InstructionMeta(code, name) {
        size += sizeof(T1);
    }
    virtual void fieldsFromStream(istream &si, void *ins) {
        ins = readField<T1>(si, ins);
    }
    virtual void fieldsToStream(ostream &so, const void *ins) {
        ins = writeField<T1>(so, ins);
    }
};
template<typename T1, typename T2>
struct InstructionMeta2: public InstructionMeta {
    InstructionMeta2(int code, const string &name): InstructionMeta(code, name) {
        size += sizeof(T1) + sizeof(T2);
    }
    virtual void fieldsFromStream(istream &si, void *ins) {
        ins = readField<T1>(si, ins);
        ins = readField<T2>(si, ins);
    }
    virtual void fieldsToStream(ostream &so, const void *ins) {
        ins = writeField<T1>(so, ins);
        ins = writeField<T2>(so, ins);
    }
};
template<typename T1, typename T2, typename T3>
struct InstructionMeta3: public InstructionMeta {
    InstructionMeta3(int code, const string &name): InstructionMeta(code, name) {
        size += sizeof(T1) + sizeof(T2) + sizeof(T3);
    }
    virtual void fieldsFromStream(istream &si, void *ins) {
        ins = readField<T1>(si, ins);
        ins = readField<T2>(si, ins);
        ins = readField<T3>(si, ins);
    }
    virtual void fieldsToStream(ostream &so, const void *ins) {
        ins = writeField<T1>(so, ins);
        ins = writeField<T2>(so, ins);
        ins = writeField<T3>(so, ins);
    }
};
template<typename T1, typename T2, typename T3, typename T4>
struct InstructionMeta4: public InstructionMeta {
    InstructionMeta4(int code, const string &name): InstructionMeta(code, name) {
        size += sizeof(T1) + sizeof(T2) + sizeof(T3) + sizeof(T4);
    }
    virtual void fieldsFromStream(istream &si, void *ins) {
        ins = readField<T1>(si, ins);
        ins = readField<T2>(si, ins);
        ins = readField<T3>(si, ins);
        ins = readField<T4>(si, ins);
    }
    virtual void fieldsToStream(ostream &so, const void *ins) {
        ins = writeField<T1>(so, ins);
        ins = writeField<T2>(so, ins);
        ins = writeField<T3>(so, ins);
        ins = writeField<T4>(so, ins);
    }
};

class InstructionMetaManager {
public:
    InstructionMeta* get(const string &name) { return m_metaMap[name]; }
    InstructionMeta* get(CodeType code) { return m_metaArray[code]; }

    void add(InstructionMeta* ins) {
        m_metaMap[ins->name] = ins;
        m_metaArray.resize(ins->code + 1);
        m_metaArray[ins->code] = ins;
    }

    ~InstructionMetaManager() {
        for (int i = 0; i < (int)m_metaArray.size(); ++i) {
            delete m_metaArray[i];
        }
    }
private:
    map<string, InstructionMeta*> m_metaMap;
    vector<InstructionMeta*> m_metaArray;
};
//==============================

class InterpreterFactory;
class InstructionList {
public:
    InstructionList(InterpreterFactory *factory): m_factory(factory){}
    virtual ~InstructionList() {}

    void fromStream(istream &si);
    void toStream(ostream &so) const;

    virtual void translateJmpIdx2Off() = 0;
    virtual void appendEOF() = 0;

    vector<char>& getBytes() { return m_bytes; }
protected:
    template<typename T>
    void appendValue(const T& v) {
        int off = (int)m_bytes.size();
        m_bytes.resize(off + sizeof(v));
        memcpy(&m_bytes[off], &v, sizeof(v));
    }

protected:
    InterpreterFactory *m_factory;
    vector<char> m_bytes;
};

//==============================
class Interpreter {
public:
    Interpreter(InterpreterFactory *factory): m_factory(factory){}
    virtual ~Interpreter(){}

    virtual int interpret(InstructionList *insList) = 0;
    virtual bool isValid() = 0;
protected:
    InterpreterFactory *m_factory;
private:
};

//==============================
class InterpreterFactory {
public:
    static InterpreterFactory* getFactory(const string &name);

    virtual ~InterpreterFactory(){}

    InstructionMetaManager* getInstructionMetaManager() { return &m_insMetaMgr; }

    virtual InstructionList* createInstructionList() = 0;
    void destroyInstructionList(InstructionList* p) { delete p; }

    virtual Interpreter* createInterpreter(const string &name) = 0;
    void destroyInterpreter(Interpreter* p) { delete p; }

    int getInstructionSize(CodeType code) { return m_insMetaMgr.get(code)->size; }
    const char* getInstructionName(CodeType code) { return m_insMetaMgr.get(code)->name.c_str(); }
protected:
    InstructionMetaManager m_insMetaMgr;
};

#endif // INTERPRETER_H
