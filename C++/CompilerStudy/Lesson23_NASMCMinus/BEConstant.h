
#ifndef BE_CONSTANT_H
#define BE_CONSTANT_H

struct BEType;

struct BEConstant {
    virtual ~BEConstant(){}
    string name;
    const BEType *type;
protected:
    BEConstant(const string &_name, const BEType *_type): name(_name), type(_type){}
};
struct BEConstantString : public BEConstant {
    string str;
    BEConstantString(const string &_str = "");
};
struct BEConstantInt : public BEConstant {
    int num;
    BEConstantInt(int _num = 0);
};

class BEConstantPool {
public:
    BEConstant* get(int num);
    BEConstant* get(const string &str);
    const map<int, BEConstantInt>& getInts() const { return m_constInts; }
    const map<string, BEConstantString>& getStrings() const { return m_constStrs; }

private:
    map<int, BEConstantInt> m_constInts;
    map<string, BEConstantString> m_constStrs;
};

#endif
