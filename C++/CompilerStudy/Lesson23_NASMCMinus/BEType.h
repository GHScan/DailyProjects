
#ifndef BE_TYPE_H
#define BE_TYPE_H

struct BEType {
    string name;
    int size;
    BEType(const string& _name, int _size): name(_name), size(_size){}
    virtual ~BEType(){}
};
struct BEType_Array: public BEType {
    const BEType *elemType;
    int count;
    BEType_Array(const BEType *_elemType, int _count): 
        BEType(format("%s[%d]", _elemType->name.c_str(), _count), 4), 
        elemType(_elemType), count(_count) {}
};

class BETypeManager {
public:
    static BETypeManager* instance() {
        static BETypeManager s_ins;
        return &s_ins;
    }

    const BEType* get(const string &name) const;
    const BEType* getFunc() const;
    BETypeManager();
    ~BETypeManager();
private:
    map<string, BEType*> m_types;
};

#endif
