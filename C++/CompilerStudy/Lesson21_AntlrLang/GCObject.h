
#ifndef GC_OBJECT_H
#define GC_OBJECT_H

struct GCObject {
    enum GCObjectType {
        GCT_String,
        GCT_Array,
    };
    GCObjectType type;

protected:
    GCObject(GCObjectType _type): type(_type){}
private:
    GCObject& operator = (const GCObject &);
    GCObject(const GCObject& );
};

class GCObjectManager {
public:
    void link(GCObject *obj);
    void performFullGC();

    static GCObjectManager* instance() {
        static GCObjectManager s_ins;
        return &s_ins;
    }

private:
    GCObjectManager(){}
    GCObjectManager(const GCObjectManager&);
    GCObjectManager& operator = (const GCObjectManager&);
};

#endif
