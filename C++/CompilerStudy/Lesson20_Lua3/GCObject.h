
#ifndef GC_OBJECT_H
#define GC_OBJECT_H

struct GCObject {
    enum GCState {
        GCS_Unaccess, GCS_Unscan, GCS_Scaned,
    };
    enum ObjType {
        OT_String, OT_Table, OT_Function, OT_Stack,
    };

    GCObject(ObjType _objType): next(NULL), gcState(GCS_Unaccess), objType(_objType){}
    GCObject* gcAccess() {
        if (gcState == GCS_Unaccess) {
            gcState = GCS_Unscan;
            return this;
        }
        return NULL;
    }

    GCObject *next;
    GCState gcState;
    const ObjType objType;
};

class GCObjectManager {
public:
    GCObjectManager();
    ~GCObjectManager();

    void performFullGC();
    void linkObject(GCObject *obj);
    int getObjCount() const { return m_objCount;}

private:
    GCObjectManager(const GCObjectManager&);
    GCObjectManager& operator = (const GCObjectManager&);

private:
    GCObject *m_headObj;
    int m_objCount;
};

#endif
