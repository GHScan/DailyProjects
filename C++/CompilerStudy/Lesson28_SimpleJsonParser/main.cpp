#include "pch.h" 

#include <stdint.h>
#include <math.h>
#include <string.h>
#include <time.h>
#include <assert.h>

#include <string>
#include <vector>
#include <iostream>
#include <typeinfo>
using namespace std;

#include <memory>
#include <unordered_map>

static void printSpace(ostream& so, int n) {
    for (int i = 0; i < n; ++i) so << '\t';
}

struct JsonNode {
    virtual ~JsonNode(){}
    virtual void print(ostream& so, int depth) = 0;
};
typedef shared_ptr<JsonNode> JsonNodePtr;

struct JsonNode_Null: public JsonNode {
    virtual void print(ostream& so, int depth) {
        so << "null";
    }
};
struct JsonNode_Boolean: public JsonNode {
    bool b;
    JsonNode_Boolean(bool _b): b(_b){}
    virtual void print(ostream& so, int depth) {
        so << (b ? "true" : "false");
    }
};
struct JsonNode_String: public JsonNode {
    string str;
    explicit JsonNode_String(const string &_str): str(_str){}
    virtual void print(ostream& so, int depth) {
        so << '"' << str << '"';
    }
};
struct JsonNode_Number: public JsonNode {
    double num;
    explicit JsonNode_Number(double _num): num(_num) {}
    virtual void print(ostream& so, int depth) {
        so << num;
    }
};
struct JsonNode_Array: public JsonNode {
    vector<JsonNodePtr> array;
    virtual void print(ostream& so, int depth) {
        so << "[\n";
        for (int i = 0; i < (int)array.size(); ++i) {
            printSpace(so, depth + 1);
            array[i]->print(so, depth + 1);
            so << ",\n";
        }
        printSpace(so, depth); so << ']';
    }
};
struct JsonNode_HashTable: public JsonNode {
    unordered_map<JsonNodePtr, JsonNodePtr> hashtable;
    virtual void print(ostream& so, int depth) {
        so << "{\n";
        for (auto iter = hashtable.begin(); iter != hashtable.end(); ++iter) {
            printSpace(so, depth + 1);
            iter->first->print(so, depth + 1);
            so << " : ";
            iter->second->print(so, depth + 1);
            so << ",\n";
        }
        printSpace(so, depth); so << '}';
    }
};

static string unescape(const char *begin, const char *end) {
    string r;
    for (; begin != end; ++begin) {
        if (begin[0] == '\\') {
            switch (begin[1]) {
                case 't': r.push_back('\t'); break;
                case 'n': r.push_back('\n'); break;
                case 'x': assert(0); break;
                case '0': assert(0); break;
                default: r.push_back(begin[1]); break;
            }
            ++begin;
        } else {
            r.push_back(begin[0]);
        }
    }
    return r;
}

static void skipSpace(const char *&str) {
    while (isspace(str[0])) ++str;
}
static JsonNodePtr parse_value(const char *&str);
static bool parse_char(const char *&str, char c) {
    skipSpace(str);
    if (str[0] == c) return ++str, true;
    return false;
}
static JsonNodePtr parse_boolean(const char *&str) {
    skipSpace(str);
    if (strncmp(str, "true", 4) == 0) return str += 4, JsonNodePtr(new JsonNode_Boolean(true));
    else if (strncmp(str, "false", 5) == 0) return str += 5, JsonNodePtr(new JsonNode_Boolean(false));
    return nullptr;
}
static JsonNodePtr parse_null(const char *&str) {
    skipSpace(str);
    if (strncmp(str, "null", 4) == 0) return str += 4, JsonNodePtr(new JsonNode_Null());
    return nullptr;
}
static JsonNodePtr parse_string(const char *&str) {
    if (!parse_char(str, '\'')) return nullptr;
    const char *begin = str;
    while (str[0] && str[0] != '\'') {
        if (str[0] == '\\' && str[1]) str += 2;
        else ++str;
    }
    const char *end = str;
    if (!parse_char(str, '\'')) return nullptr;
    return JsonNodePtr(new JsonNode_String(unescape(begin, end)));
}
static JsonNodePtr parse_number(const char *&str) {
    skipSpace(str);
    char *strend;
    double v = strtod(str, &strend);
    if (str == strend) return nullptr;
    str = strend;
    return JsonNodePtr(new JsonNode_Number(v));
}
static JsonNodePtr parse_array(const char *&str) {
    if (!parse_char(str, '[')) return nullptr;
    JsonNode_Array *a = new JsonNode_Array();
    JsonNodePtr r(a);
    while (JsonNodePtr p = parse_value(str)) {
        a->array.push_back(p);
        if (!parse_char(str, ',')) break;
    }
    if (!parse_char(str, ']')) return nullptr;
    return r;
}
static JsonNodePtr parse_hashtable(const char *&str) {
    if (!parse_char(str, '{')) return nullptr;
    JsonNode_HashTable *h = new JsonNode_HashTable();
    JsonNodePtr r(h);
    while (JsonNodePtr key = parse_string(str)) {
        if (!parse_char(str, ':')) return nullptr;
        JsonNodePtr value = parse_value(str);
        if (!value) return nullptr;
        h->hashtable[key] = value;
        if (!parse_char(str, ',')) break;
    }
    if (!parse_char(str, '}')) return nullptr;
    return r;
}
static JsonNodePtr parse_value(const char *&str) {
    if (JsonNodePtr p = parse_null(str)) return p;
    if (JsonNodePtr p = parse_boolean(str)) return p;
    if (JsonNodePtr p = parse_number(str)) return p;
    if (JsonNodePtr p = parse_string(str)) return p;
    if (JsonNodePtr p = parse_array(str)) return p;
    if (JsonNodePtr p = parse_hashtable(str)) return p;
    return nullptr;
}
static JsonNodePtr parse_json(const char *str) {
    return parse_hashtable(str);
}

int main() {
    JsonNodePtr p = parse_json("{'a':true, 'array': [null, 1, 2, 3, 4, {'a':'b', 'c': 'd', 'e':[3,6,9]}]}");
    if (p) p->print(cout, 0);
}
