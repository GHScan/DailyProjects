#include <cstdint>
#include <string>
using namespace std;

static inline uint64_t Djb2(uint8_t const *bytes, size_t length)
{
    uint64_t hashCode = 5381;
    for (size_t i = 0; i < length; ++i)
        hashCode = ((hashCode << 5) + hashCode) + bytes[i];
    return hashCode;
}

template<typename TKey>
static inline uint64_t Djb2(TKey v, typename enable_if<is_pod<TKey>::value>::type* = nullptr)
{
    return Djb2(reinterpret_cast<uint8_t const*>(&v), sizeof(v));
}

static inline uint64_t Djb2(string const &v)
{
    return Djb2(reinterpret_cast<uint8_t const*>(v.c_str()), v.size());
}

static inline uint64_t Fnv(uint8_t const *bytes, size_t length)
{
    auto fnvBasis = 14695981039346656037ULL;
    auto fnvPrime = 1099511628211ULL;
    auto hashCode = fnvBasis;
    for (size_t i = 0; i < length; ++i)
        hashCode = (hashCode ^ bytes[i]) * fnvPrime;
    return hashCode;
}

template<typename TKey>
static inline uint64_t Fnv(TKey v, typename enable_if<is_pod<TKey>::value>::type* = nullptr)
{
    return Fnv(reinterpret_cast<uint8_t const*>(&v), sizeof(v));
}

static inline uint64_t Fnv(string const &v)
{
    return Fnv(reinterpret_cast<uint8_t const*>(v.c_str()), v.size());
}

size_t HashToken(string const &key)
{
     auto h1 = Djb2(key);
     auto h2 = Fnv(key);
     switch (h1 % 29)
     {
        case 0:return (h1 + h2 *169861039 + 2834429280) % 97;
        case 1:return (h1 + h2 *1272704213 + 4283041477) % 97;
        case 2:return (h1 + h2 *2248850472 + 2838390091) % 97;
        case 3:return (h1 + h2 *874715008 + 641472548) % 97;
        case 4:return (h1 + h2 *3685865166 + 4124995253) % 97;
        case 5:return (h1 + h2 *874715008 + 641472548) % 97;
        case 6:return (h1 + h2 *3846521850 + 3284210406) % 97;
        case 7:return (h1 + h2 *3685865166 + 4124995253) % 97;
        case 8:return (h1 + h2 *2687914748 + 1581619815) % 97;
        case 9:return (h1 + h2 *874715008 + 641472548) % 97;
        case 10:return (h1 + h2 *2248850472 + 2838390091) % 97;
        case 11:return (h1 + h2 *3774009393 + 3343731254) % 97;
        case 12:return (h1 + h2 *2248850472 + 2838390091) % 97;
        case 13:return (h1 + h2 *456094204 + 3381320346) % 97;
        case 14:return (h1 + h2 *874715008 + 641472548) % 97;
        case 15:return (h1 + h2 *1657501171 + 84618794) % 97;
        case 16:return (h1 + h2 *3399013286 + 761880097) % 97;
        case 17:return (h1 + h2 *3218326340 + 1719090551) % 97;
        case 18:return (h1 + h2 *1272704213 + 4283041477) % 97;
        case 19:return (h1 + h2 *1272704213 + 4283041477) % 97;
        case 20:return (h1 + h2 *129905737 + 2067184489) % 97;
        case 21:return (h1 + h2 *3218326340 + 1719090551) % 97;
        case 22:return (h1 + h2 *3846521850 + 3284210406) % 97;
        case 23:return (h1 + h2 *456094204 + 3381320346) % 97;
        case 24:return (h1 + h2 *2248850472 + 2838390091) % 97;
        case 25:return (h1 + h2 *1603017585 + 692914850) % 97;
        case 26:return (h1 + h2 *3231666612 + 301925943) % 97;
        case 27:return (h1 + h2 *2248850472 + 2838390091) % 97;
        case 28:return (h1 + h2 *129905737 + 2067184489) % 97;
     }
   return 0;
}
size_t HashToken_slotCount = 97;