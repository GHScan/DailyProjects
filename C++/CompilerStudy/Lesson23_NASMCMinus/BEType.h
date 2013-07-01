
#ifndef BE_TYPE_H
#define BE_TYPE_H

struct BEType {
    BEType(): size(0) {}
    BEType(const string& _typeName, int _size): typeName(_typeName), size(_size){}

    string typeName;
    int size;
};

class BETypeManager {
public:
    static BETypeManager* instance() {
        static BETypeManager s_ins;
        return &s_ins;
    }

    const BEType* getType(const string &typeName) const;
    BETypeManager();
    ~BETypeManager();
private:
    map<string, BEType> m_types;
};

#endif
