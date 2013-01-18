// vim:fileencoding=gbk

#include "pch.h"

#include <cassert>

#include <string>
#include <map>
#include <vector>

using namespace std;

template<typename DT, typename ST>
DT force_cast(ST s)
{
    assert(sizeof(ST) == sizeof(DT));
    union 
    {
        ST a;
        DT b;
    } r = {s};
    return r.b;
}

class FieldDescriptor
{
public:
    FieldDescriptor(): m_off(0) {}
    FieldDescriptor(const string& name, int off): m_name(name), m_off(off) {}
    const string name() const { return m_name; }
    int offset() const { return m_off; }
private:
    string  m_name;
    int     m_off;
};

class MethodDescriptor
{
public:
    MethodDescriptor(): m_func(NULL){}
    MethodDescriptor(const string& name, void *func): m_name(name), m_func(func) {}
    const string name() const { return m_name; }
    void *func() const { return m_func; }

private:
    string m_name;
    void *m_func;
};

class FieldInvoker
{
public:
    FieldInvoker(void* owner, const FieldDescriptor* descriptor):m_descriptor(descriptor), m_owner(owner) {}
    const string name() const { return m_descriptor->name(); }
    template<typename T>
    void set(const T& val)
    {
        *(T*)((char*)m_owner + m_descriptor->offset()) = val;
    }
    template<typename T>
    const T& get() const
    {
        return *(const T*)((char*)m_owner + m_descriptor->offset());
    }
private:
    const FieldDescriptor *m_descriptor;
    void *m_owner;
};

class MethodInvoker
{
public:
    MethodInvoker(void *owner, const MethodDescriptor* descriptor): m_owner(owner), m_descriptor(descriptor) {}
    const string name() const { return m_descriptor->name(); }
    template<typename R>
    R invoke() const 
    {
        return (((MethodInvoker*)m_owner)->*force_cast<R(MethodInvoker::*)()>(m_descriptor->func()))();
    }
    template<typename R, typename T0>
    R invoke(const T0& a0) const 
    {
        return (((MethodInvoker*)m_owner)->*force_cast<R(MethodInvoker::*)(const T0&)>(m_descriptor->func()))(a0);
    }
    template<typename R, typename T0, typename T1>
    R invoke(const T0& a0, const T1& a1) const 
    {
        return (((MethodInvoker*)m_owner)->*force_cast<R(MethodInvoker::*)(const T0&, const T1&)>(m_descriptor->func()))(a0, a1);
    }
private:
    void *m_owner;
    const MethodDescriptor *m_descriptor;
};

class TypeDescriptor
{
private:
    typedef map<string, FieldDescriptor>    FieldMap;
    typedef map<string, MethodDescriptor>   MethodMap;
public:
    TypeDescriptor(const TypeDescriptor* parent, const string& name): m_parent(parent), m_name(name) {}
    const string name() const { return m_name; }
    const TypeDescriptor* parent() const { return m_parent; }

    void addField(const FieldDescriptor& field) { assert(!m_fields.count(field.name())); m_fields[field.name()] = field; }
    void addMethod(const MethodDescriptor& method) { assert(!m_methods.count(method.name())); m_methods[method.name()] = method; }

    const FieldDescriptor* getField(const string& name) const 
    {
        FieldMap::const_iterator iter = m_fields.find(name);
        if (iter == m_fields.end()) return NULL;
        return &iter->second;
    }
    const MethodDescriptor* getMethod(const string& name) const 
    {
        MethodMap::const_iterator iter = m_methods.find(name);
        if (iter == m_methods.end()) return NULL;
        return &iter->second;
    }
    vector<string> getFieldList() const 
    {
        vector<string> v;
        for (FieldMap::const_iterator iter = m_fields.begin();
                iter != m_fields.end(); ++iter) v.push_back(iter->first);
        return v;
    }
    vector<string> getMethodList() const 
    {
        vector<string> v;
        for (MethodMap::const_iterator iter = m_methods.begin();
                iter != m_methods.end(); ++iter) v.push_back(iter->first);
        return v;
    }

private:
    const TypeDescriptor* m_parent;
    string m_name;
    FieldMap m_fields;
    MethodMap m_methods;
};

class Type
{
public:
    Type(void* owner, const TypeDescriptor* descriptor): m_owner(owner), m_descriptor(descriptor){}
    FieldInvoker getField(const string& name)
    {
        const TypeDescriptor* d = m_descriptor;
        while (d != NULL) {
            const FieldDescriptor *p = d->getField(name);
            if (p != NULL) return FieldInvoker(m_owner, p);
            d = d->parent();
        }
        return FieldInvoker(m_owner, NULL);
    }
    MethodInvoker getMethod(const string& name) 
    {
        const TypeDescriptor *d = m_descriptor;
        while (d != NULL) {
            const MethodDescriptor *p = d->getMethod(name);
            if (p != NULL) return MethodInvoker(m_owner, p);
            d = d->parent();
        }
        return MethodInvoker(m_owner, NULL);
    }
    vector<string> getFieldList() const { return m_descriptor->getFieldList(); }
    vector<string> getMethodList() const { return m_descriptor->getMethodList(); }
private:
    void *m_owner;
    const TypeDescriptor* m_descriptor;
};

#define TYPEDESC_BEGIN2(cls, parentCls)  TypeDescriptor cls::initTypeDescriptor() { TypeDescriptor descriptor(parentCls::getTypeDescriptor(), #cls);
#define TYPEDESC_BEGIN(cls)   TypeDescriptor cls::initTypeDescriptor() { TypeDescriptor descriptor(NULL, #cls);
#define ADD_FIELD(cls, field) descriptor.addField(FieldDescriptor(#field, force_cast<int>(&cls::field)));
#define ADD_METHOD(cls, method) descriptor.addMethod(MethodDescriptor(#method, force_cast<void*>(&cls::method)));
#define TYPEDESC_END() return descriptor; }

#define DECLARE_TYPEDESC(cls)  private: static TypeDescriptor s_typeDescriptor; static TypeDescriptor initTypeDescriptor();  public: static TypeDescriptor* getTypeDescriptor(); virtual Type getType();
#define IMPLEMENT_TYPEDESC(cls) TypeDescriptor cls::s_typeDescriptor = cls::initTypeDescriptor(); TypeDescriptor* cls::getTypeDescriptor() { return &s_typeDescriptor; } Type cls::getType() { return Type(this, getTypeDescriptor()); }

class A
{
    string m_name;
    int m_id;
public:
    string& name() { return m_name; }
    const string getName() const { return m_name; }
    int& id() { return m_id; }
    DECLARE_TYPEDESC(A)
};
IMPLEMENT_TYPEDESC(A)
TYPEDESC_BEGIN(A)
    ADD_FIELD(A, m_name)
    ADD_FIELD(A, m_id)
    ADD_METHOD(A, name)
    ADD_METHOD(A, getName)
TYPEDESC_END()

class B:
    public A
{
    string m_school;
public:
    string& school() { return m_school; }
    DECLARE_TYPEDESC(B)
};
IMPLEMENT_TYPEDESC(B)
TYPEDESC_BEGIN2(B, A)
    ADD_FIELD(B, m_school)
TYPEDESC_END()

int main()
{
    B a;

    a.name() = "fd";
    string s = (&a->*force_cast<string&(A::*)()>(force_cast<void*>(&A::name)))();

    Type t = a.getType();
    a.name() = "1434:";
    cout << a.name() << endl;
    t.getField("m_name").set<string>("wefds");
    cout << t.getField("m_name").get<string>() << endl;
    a.id() = 9876;
    cout << t.getField("m_id").get<int>() << endl;

    t.getField("m_school").set<string>("china");
    cout << a.school() << endl;

    t.getMethod("name").invoke<string&>() = "method test";
    cout << t.getMethod("getName").invoke<string>() << endl;
}
