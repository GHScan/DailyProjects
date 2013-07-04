
#ifndef BE_CONSTANT_H
#define BE_CONSTANT_H

struct BEType;

struct BEConstant {
    virtual ~BEConstant(){}
    const BEType *type;
protected:
    BEConstant(const BEType *_type): type(_type){}
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

private:
    map<int, BEConstantInt> m_constInts;
    map<string, BEConstantString> m_constStrs;
};

#endif
