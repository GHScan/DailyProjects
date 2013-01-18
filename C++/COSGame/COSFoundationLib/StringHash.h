#pragma once

#include <hash_set>

#include <boost/unordered_set.hpp>

#include "TypeDefine.h"

namespace Scan
{

template<typename StringT, typename HashT>
class StringHashT:
    private Copyable<true>
{
public:
    typedef StringT     StringT;
    typedef HashT       HashT;

    enum HashAlgo
    {
        HA_Null,
        HA_Stdext,
        HA_Boost,
    };

public:
    StringHashT(): m_hash(HashT()), m_hashAlgo(HA_Null){}
    explicit StringHashT(const StringT& str): m_str(str), m_hash(HashT()), m_hashAlgo(HA_Null) {}

    bool operator < (const StringHashT& o) const { return m_str < o.m_str;}
    bool operator == (const StringHashT& o) const { return m_str == o.m_str; }

    const StringT& getString() const { return m_str; }
    void setString(const StringT& s) 
    {
        if (s != m_str) m_hashAlgo = HA_Null;
        m_str = s;
    }

    const HashT getHash() const { return m_hash; }
    HashAlgo getHashAlgo() const { return m_hashAlgo; }
    void setHash(const HashT hash, HashAlgo algo) { m_hash = hash; m_hashAlgo = algo; }

private:
    StringT     m_str;
    HashT       m_hash;
    HashAlgo    m_hashAlgo;
};  

typedef StringHashT<std::string, size_t>    StringHash;

}

namespace stdext
{
    inline size_t hash_value(const Scan::StringHash& sh)
    {
        if (sh.getHashAlgo() != Scan::StringHash::HA_Stdext)
        {
            Scan::StringHash& _sh = const_cast<Scan::StringHash&>(sh);
            _sh.setHash(hash_value(_sh.getString()), Scan::StringHash::HA_Stdext);
        }
        return sh.getHash();
    }
}

namespace boost
{
    template <> struct hash<Scan::StringHash>
    : std::unary_function<Scan::StringHash, std::size_t>
    {
        std::size_t operator()(Scan::StringHash const& sh) const
        {
            if (sh.getHashAlgo() != Scan::StringHash::HA_Boost)
            {
                Scan::StringHash& _sh = const_cast<Scan::StringHash&>(sh);
                _sh.setHash(hash<Scan::StringHash::StringT>()(_sh.getString()), Scan::StringHash::HA_Boost);
            }
            return sh.getHash();
        }
    };
}