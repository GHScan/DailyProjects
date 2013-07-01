
#ifndef ID_GENERATOR_H
#define ID_GENERATOR_H

class IDGenerator {
public:
    static IDGenerator* instance() {
        static IDGenerator s_ins;
        return &s_ins;
    }

    IDGenerator(): m_id(0){}
    int generateID() {
        return ++m_id;
    }
    string generateName(const string &prefix) {
        return format("%s_%d", prefix.c_str(), generateID());
    }
private:
    int m_id;
};

#endif
