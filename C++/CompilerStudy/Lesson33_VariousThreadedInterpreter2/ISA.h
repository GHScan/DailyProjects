#ifndef ISA_H
#define ISA_H

#define CodeSize 4
#define LocalIdxSize 4
#define JmpOffSize 4
#define EvalStackSize 32
#define LocalStackSize 64

template<int size>
struct FixedSizeType {
    typedef char Type;
};
template<>
struct FixedSizeType<2> {
    typedef short Type;
};
template<>
struct FixedSizeType<4> {
    typedef int Type;
};
template<>
struct FixedSizeType<8> {
    typedef long long Type;
};

typedef FixedSizeType<CodeSize>::Type CodeType;
typedef FixedSizeType<LocalIdxSize>::Type LocalIdxType;
typedef FixedSizeType<JmpOffSize>::Type JmpOffType;

typedef char Byte;

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

template<typename CodeCategory>
class InstructionMetaManager {
public:
    InstructionMeta* get(const string &name) { return m_metaMap[name]; }
    InstructionMeta* get(CodeType code) { return m_metaArray[code]; }

    void add(InstructionMeta* ins) {
        m_metaMap[ins->name] = ins;
        m_metaArray.resize(ins->code + 1);
        m_metaArray[ins->code] = ins;
    }

    static InstructionMetaManager* instance() {
        static InstructionMetaManager s_ins;
        return &s_ins;
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

#endif // ISA_H
