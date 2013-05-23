
#ifndef GC_OBJECT_H
#define GC_OBJECT_H

struct GCObject {
    enum GCObjectType {
        GCT_String,
        GCT_Array,
        GCT_Function,
    };
    enum GCState {
        GCS_Unaccess,
        GCS_Unscan,
        GCS_Scan,
    };
    GCObjectType type;
    mutable GCState state;
    GCObject *next;

    GCObject* gcAccess() const {
        if (state == GCS_Unaccess) {
            state = GCS_Unscan;
            return (GCObject*)this;
        }
        return NULL;
    }

protected:
    GCObject(GCObjectType _type): type(_type), state(GCS_Unaccess){}

private:
    GCObject& operator = (const GCObject &);
    GCObject(const GCObject& );
};

class GCObjectManager {
public:
    void link(GCObject *obj);
    void performFullGC();
    int getObjectCount() const { return m_objCount; }

    static void createInstance() { s_ins = new GCObjectManager(); }
    static void destroyInstance() { delete s_ins; }
    static GCObjectManager* instance() { return s_ins; }

private:
    GCObject *m_head;
    int m_objCount;

private:
    GCObjectManager(): m_head(NULL), m_objCount(0) {}
    ~GCObjectManager();

    GCObjectManager(const GCObjectManager&);
    GCObjectManager& operator = (const GCObjectManager&);

    static GCObjectManager *s_ins;
};

#endif
