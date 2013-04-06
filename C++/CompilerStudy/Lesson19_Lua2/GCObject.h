
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

    GCObject *next;
    GCState gcState;
    const ObjType objType;
};

class GCObjectManager {
public:
    GCObjectManager();
    ~GCObjectManager();

    void performFullGC();
    // TODO: makesure kinds of gcobject has call this
    void linkObject(GCObject *obj);

private:
    GCObjectManager(const GCObjectManager&);
    GCObjectManager& operator = (const GCObjectManager&);

private:
    GCObject *m_headObj;
};

#endif
