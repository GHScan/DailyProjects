
#include "stdafx.h"

#include <cassert>
#include <cstdint>

#include <string>
#include <vector>
#include <algorithm>
#include <unordered_map>
#include <map>
#include <random>
#include <iostream>
#include <chrono>

#include <windows.h>
#undef max

//------------------------------------------------------------
class symbol final {
public:
    symbol(std::string const& str)
        : str_(str), hash_code_(0) {
    }

    symbol(symbol const&) = default;
    symbol& operator =(symbol const&) = default;
    symbol(symbol&&) = default;
    symbol& operator =(symbol&&) = default;

    bool operator ==(symbol const& rhs) const {
        return hash_code() == rhs.hash_code() && str() == rhs.str();
    }

    bool operator <(symbol const& rhs) const {
        return str() < rhs.str();
    }

    std::string const& str() const {
        return str_;
    }

    size_t hash_code() const {
        if (hash_code_ == 0) {
            hash_code_ = std::hash<std::string>()(str());
        }
        return hash_code_;
    }

private:
    std::string str_;
    mutable size_t hash_code_;
};

namespace std {
    template <>
    struct hash<symbol> {
        size_t operator()(symbol const& v) const {
            return v.hash_code();
        }
    };
}

class id16_manager final {
public:
    static id16_manager* instance() {
        static id16_manager s_instance;
        return &s_instance;
    }

    uint16_t get_id(std::string const& str) {
        auto it = str_2_id_.lower_bound(str);
        if (it != str_2_id_.end() && it->first == str) {
            return it->second;
        }
        else {
            it = str_2_id_.insert(
                it,
                std::make_pair(str, static_cast<uint16_t>(str_2_id_.size())));
            id_2_str_.push_back(&(it->first));
            return it->second;
        }
    }

    bool try_get_id(std::string const& str, uint16_t& id) {
        auto it = str_2_id_.find(str);
        if (it != str_2_id_.end()) {
            id = it->second;
            return true;
        }
        return false;
    }

    std::string const& get_str(uint16_t id) const {
        return *id_2_str_[id];
    }

private:
    id16_manager() {
    }

private:
    std::map<std::string, uint16_t> str_2_id_;
    std::vector<std::string const*> id_2_str_;
};

class id16 final {
public:
    id16(std::string const& str)
        : id_(id16_manager::instance()->get_id(str)) {
    }

    bool operator ==(id16 const& rhs) const {
        return id() == rhs.id();
    }

    bool operator <(id16 const& rhs) const {
        return id() < rhs.id();
    }

    uint16_t id() const {
        return id_;
    }

    size_t hash_code() const {
        return id_;
    }

private:
    uint16_t id_;
};

namespace std {
    template <>
    struct hash<id16> {
        size_t operator()(id16 v) const {
            return v.hash_code();
        }
    };
}

//------------------------------------------------------------
__forceinline
static size_t index_of_tiny(uint16_t const* array, size_t length, uint16_t value) {
    size_t i = 0;

    DWORD index;
    auto values = _mm_set1_epi16(value);
    for (; i + 16 <= length; i += 16) {
        auto a = _mm_loadu_si128(reinterpret_cast<const __m128i *>(array + i));
        auto b = _mm_loadu_si128(reinterpret_cast<const __m128i *>(array + i + 8));
        auto flag = _mm_movemask_epi8(_mm_packs_epi16(_mm_cmpeq_epi16(a, values), _mm_cmpeq_epi16(b, values)));
        if (_BitScanForward(&index, flag)) {
            return i + index;
        }
    }

    for (; i < length; ++i)
        if (array[i] == value) return i;

    return length;
}

static size_t index_of_sse2(uint16_t const* array, size_t length, uint16_t value) {
    if (length < 32 + 16) {
        return index_of_tiny(array, length, value);
    }

    size_t i = 0;

    for (auto count = ((16 - reinterpret_cast<uintptr_t>(array) & 0xf) & 0xf) / sizeof(int16_t);
    i < count;
        ++i) {
        if (array[i] == value) return i;
    }

    DWORD index;

    _mm_prefetch(reinterpret_cast<char const *>(array + i), _MM_HINT_T0);
    auto values = _mm_set1_epi16(value);
    for (; i + 32 <= length; i += 32) {
        _mm_prefetch(reinterpret_cast<char const *>(array + i + 32), _MM_HINT_T0);
        auto a = _mm_load_si128(reinterpret_cast<const __m128i *>(array + i));
        auto b = _mm_load_si128(reinterpret_cast<const __m128i *>(array + i + 8));
        auto c = _mm_load_si128(reinterpret_cast<const __m128i *>(array + i + 16));
        auto d = _mm_load_si128(reinterpret_cast<const __m128i *>(array + i + 24));
        auto flag1 = _mm_movemask_epi8(_mm_packs_epi16(_mm_cmpeq_epi16(a, values), _mm_cmpeq_epi16(b, values)));
        auto flag2 = _mm_movemask_epi8(_mm_packs_epi16(_mm_cmpeq_epi16(c, values), _mm_cmpeq_epi16(d, values)));
        if (_BitScanForward(&index, (flag2 << 16) | flag1)) {
            return i + index;
        }
    }

    return i + index_of_tiny(array + i, length - i, value);
}

static size_t index_of_avx(uint16_t const* array, size_t length, uint16_t value) {
    static uint8_t s_index_map[] = {
        0, 1, 2, 3, 4, 5, 6, 7,
        16, 17, 18, 19, 20, 21, 22, 23,
        8, 9, 10, 11, 12, 13, 14, 15,
        24, 25, 26, 27, 28, 29, 30, 31
    };

    if (length < 32 + 32) {
        return index_of_tiny(array, length, value);
    }

    size_t i = 0;
    for (auto count = ((32 - reinterpret_cast<uintptr_t>(array) & 0x1f) & 0x1f) / sizeof(int16_t);
    i < count;
        ++i) {
        if (array[i] == value) return i;
    }

    DWORD index;

    _mm_prefetch(reinterpret_cast<char const *>(array + i), _MM_HINT_T0);
    auto values = _mm256_set1_epi16(value);
    for (; i + 32 <= length; i += 32) {
        _mm_prefetch(reinterpret_cast<char const *>(array + i + 32), _MM_HINT_T0);
        auto a = _mm256_load_si256(reinterpret_cast<const __m256i *>(array + i));
        auto b = _mm256_load_si256(reinterpret_cast<const __m256i *>(array + i + 16));
        auto flag = _mm256_movemask_epi8(_mm256_packs_epi16(_mm256_cmpeq_epi16(a, values), _mm256_cmpeq_epi16(b, values)));
        if (_BitScanForward(&index, flag)) {
            return i + s_index_map[index];
        }
    }

    return i + index_of_tiny(array + i, length - i, value);
}

//------------------------------------------------------------
struct standard_list_find_method final {
    template <typename TIt, typename TToken>
    static TIt find(TIt begin, TIt end, TToken token) {
        return std::find(begin, end, token);
    }
};

struct naive_list_find_method final {
    template <typename TIt>
    static TIt find(TIt begin, TIt end, id16 token) {
        for (auto it = begin; it != end; ++it)
            if (*it == token) return it;
        return end;
    }
};

struct sse2_list_find_method final {
    template <typename TIt>
    static TIt find(TIt begin, TIt end, id16 token) {
        if (begin == end) return end;
        return begin +
            index_of_sse2(reinterpret_cast<uint16_t const*>(&*begin), end - begin, token.id());
    }
};

struct avx_list_find_method final {
    template <typename TIt>
    static TIt find(TIt begin, TIt end, id16 token) {
        if (begin == end) return end;
        return begin +
            index_of_avx(reinterpret_cast<uint16_t const*>(&*begin), end - begin, token.id());
    }
};

template <typename TToken, typename TFindMethod>
class list_table final {
public:
    typedef TToken Token;

    TToken get_token(std::string const& key) {
        return TToken(key);
    }

    void insert(TToken const& token, int value) {
        auto it = TFindMethod::find(keys_.begin(), keys_.end(), token);

        if (it != keys_.end()) {
            values_[it - keys_.begin()] = value;
        }
        else {
            keys_.push_back(token);
            values_.push_back(value);
        }
    }

    int find(TToken const& token) const {
        auto it = TFindMethod::find(keys_.begin(), keys_.end(), token);

        return it == keys_.end() ? -1 : values_[it - keys_.begin()];
    }

private:
    std::vector<TToken> keys_;
    std::vector<int> values_;
};

template <typename TToken>
class ordered_list_table final {
public:
    typedef TToken Token;

    TToken get_token(std::string const& key) {
        return TToken(key);
    }

    void insert(TToken const& token, int value) {
        auto it = std::lower_bound(keys_.begin(), keys_.end(), token);

        if (it != keys_.end() && *it == token) {
            values_[it - keys_.begin()] = value;
        }
        else {
            values_.insert(values_.begin() + (it - keys_.begin()), value);
            keys_.insert(it, token);
        }
    }

    int find(TToken const& token) const {
        auto it = std::lower_bound(keys_.begin(), keys_.end(), token);

        return it != keys_.end() && *it == token ? values_[it - keys_.begin()] : -1;
    }

private:
    std::vector<TToken> keys_;
    std::vector<int> values_;
};

template <typename TToken>
class hash_table final {
public:
    typedef TToken Token;

    TToken get_token(std::string const& key) {
        return key;
    }

    void insert(TToken const& token, int value) {
        map_[token] = value;
    }

    int find(TToken const& token) const {
        auto it = map_.find(token);
        return it == map_.end() ? -1 : it->second;
    }

private:
    std::unordered_map<TToken, int> map_;
};

//------------------------------------------------------------
using string_list_table = list_table<std::string, standard_list_find_method>;
using string_ordered_list_table = ordered_list_table<std::string>;
using string_hash_table = hash_table<std::string>;
using symbol_list_table = list_table<symbol, standard_list_find_method>;
using symbol_ordered_list_table = ordered_list_table<symbol>;
using symbol_hash_table = hash_table<symbol>;
using id16_list_table = list_table<id16, standard_list_find_method>;
using id16_list_table_naive = list_table<id16, naive_list_find_method>;
using id16_list_table_sse2 = list_table<id16, sse2_list_find_method>;
using id16_list_table_avx = list_table<id16, avx_list_find_method>;
using id16_ordered_list_table = ordered_list_table<id16>;
using id16_hash_table = hash_table<id16>;

//------------------------------------------------------------
template <typename TTable>
static void test_table(
    std::vector<std::string> const& keys, std::vector<std::string> const& queries) {

    string_hash_table standard_table;
    TTable table;
    for (size_t i = 0; i < keys.size(); ++i) {
        standard_table.insert(keys[i], i);
        table.insert(keys[i], i);
    }

    for (auto& query : queries) {
        assert(table.find(query) == standard_table.find(query));
    }
}

static void test() {
    std::random_device device;
    std::mt19937 engine(device());

    for (auto len = 1; len <= 512; len <<= 1) {
        std::vector<std::string> keys(len), queries(2 * len);

        std::uniform_int_distribution<> gen(0, len - 1);
        std::generate(keys.begin(), keys.end(), [&]() {
            return std::to_string(gen(engine));
        });
        std::generate(queries.begin(), queries.end(), [&]() {
            return std::to_string(gen(engine));
        });

        test_table<string_list_table>(keys, queries);
        test_table<string_ordered_list_table>(keys, queries);
        test_table<symbol_list_table>(keys, queries);
        test_table<symbol_ordered_list_table>(keys, queries);
        test_table<symbol_hash_table>(keys, queries);
        test_table<id16_list_table>(keys, queries);
        test_table<id16_list_table_naive>(keys, queries);
        test_table<id16_list_table_sse2>(keys, queries);
        test_table<id16_list_table_avx>(keys, queries);
        test_table<id16_ordered_list_table>(keys, queries);
        test_table<id16_hash_table>(keys, queries);
    }
}

//------------------------------------------------------------
template <typename TFunc>
static void timing(char const* name, int times, TFunc func) {
    if (times > 1) func();

    auto start = std::chrono::high_resolution_clock::now();
    for (auto i = 0; i < times; ++i) {
        func();
    }
    auto end = std::chrono::high_resolution_clock::now();

    std::cout << name << " : " 
        << std::chrono::duration<double, std::milli>(end - start).count() / times << " ms" 
        << std::endl;
}

template <typename TTable>
static void benchmark_table(char const* name,
    std::vector<std::string> const& keys, std::vector<std::string> const& queries) {

    TTable table;
    for (size_t i = 0; i < keys.size(); ++i)
        table.insert(keys[i], i);
    std::vector<typename TTable::Token> tokens;
    for (auto& q : queries)
        tokens.push_back(q);

    volatile int sideEffect = 0;
    timing(name, 3, [&]() {
        for (size_t i = 0; i < 1000 - keys.size(); ++i) {
            auto counter = 0;
            for (auto& token : tokens) {
                counter += table.find(token);
            }
            sideEffect += counter;
        }
    });
}

static void benchmark() {
    std::random_device device;
    std::mt19937 engine(device());

    const int repeat = 10;
    int sizes[] = { 1, 2, 4, 8, 16, 24, 32, 50, 72, 90, 120, 150, 200, 256, 300, 350, 400, 450, 512, 768 };
    for (auto size : sizes) {
        std::vector<std::string> keys(size);
        std::vector<std::string> queries;

        std::uniform_int_distribution<> gen(0, size - 1);
        std::generate(keys.begin(), keys.end(), [&]() {
            auto s = std::to_string(gen(engine));
            return s + s + s;
        });
        for (auto i = 0; i < std::max(size / 8, 1); ++i) {
            queries.push_back(keys[gen(engine)]);
        }
        for (auto i = 0; i < std::max(size / 8, 1); ++i) {
            auto s = std::to_string(gen(engine));
            auto key = s + s + s;
            queries.push_back(key);
        }
        std::shuffle(queries.begin(), queries.end(), engine);

        std::cout << "Size - " << size << std::endl;

#define BENCH(table) benchmark_table<table>("\t" #table, keys, queries);

        BENCH(string_list_table);
        BENCH(string_ordered_list_table);
        BENCH(string_hash_table);
        BENCH(symbol_list_table);
        BENCH(symbol_ordered_list_table);
        BENCH(symbol_hash_table);
        BENCH(id16_list_table);
        BENCH(id16_list_table_naive);
        BENCH(id16_list_table_sse2);
        BENCH(id16_list_table_avx);
        BENCH(id16_ordered_list_table);
        BENCH(id16_hash_table);

#undef BENCH

        std::cout << std::endl;
    }
}

//------------------------------------------------------------
int main() {

    test();

#ifdef NDEBUG
    benchmark();
#endif

    return 0;
}

