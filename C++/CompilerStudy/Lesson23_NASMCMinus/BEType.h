
#ifndef BE_TYPE_H
#define BE_TYPE_H

struct BEType {
    BEType(): size(0) {}
    BEType(const string& _name, int _size): name(_name), size(_size){}

    string name;
    int size;
};

class BETypeManager {
public:
    static BETypeManager* instance() {
        static BETypeManager s_ins;
        return &s_ins;
    }

    const BEType* getType(const string &name) const;
    BETypeManager();
    ~BETypeManager();
private:
    map<string, BEType> m_types;
};

#endif
