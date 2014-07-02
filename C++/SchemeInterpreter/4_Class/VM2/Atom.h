#ifndef ATOM_H
#define ATOM_H

#include "StringMap.h"
#include "Singleton.h"

class Atom: public StringMap<uint8_t>::Entry {
public:
    int getID() const {
        return value;
    }

    const char *c_str() const {
        return key;
    }

    Atom() = delete;
    Atom(const Atom&) = delete;
    Atom& operator = (const Atom&) = delete;
    ~Atom() = delete;

private:
};

class AtomPool: public Singleton<AtomPool> {
public:
    Atom* intern(const char *s, int id = 0) {
        int len = (int)strlen(s);
        auto entry = mMap.get(s, len);
        if (entry == nullptr) {
            entry = mMap.insert(s, len, id);
        }

        return static_cast<Atom*>(entry);
    }

    int size() const {
        return mMap.size();
    }

private:
    StringMap<uint8_t> mMap;
};

#endif
