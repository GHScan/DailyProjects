#include "pch.h" 

#include <string.h>
#include <time.h>
#include <assert.h>

#include <string>
#include <vector>
#include <unordered_set>
#include <fstream>
#include <iostream>
#include <algorithm>
#include <iterator>
using namespace std;

class Trie {
public:
    void insert(const string &str) {
        ++m_sCount;
        for (int i = 0; i < (int)str.size(); ++i) {
            _insert(_getStrPtr(str), str.c_str() + i, (int)str.size() - i);
        }
    }
    unordered_set<const char*>* get(const char* s) const {
        Node *n = m_root;
        for (; n && *s; n = n->getChild(*s), ++s);
        return n ? &n->strs : NULL;
    }
    Trie(): m_root(new Node(0)), m_sCount(0){}
    ~Trie() { m_root->destroy();}
    int getNCount() const { return m_root->getNodeCount(); }
    int getSCount() const { return m_sCount; }
private:
    struct Node {
        vector<Node*> childs;
        unordered_set<const char*> strs;
        int value;
        Node(int v): value(v) {}
        Node* getOrCreateChild(unsigned char c) {
            Node *p = getChild(c);
            if (p) return p;
            childs.push_back(p = new Node(c));
            return p;
        }
        Node* getChild(unsigned char c) {
            for (int i = 0; i < (int)childs.size(); ++i) {
                if (childs[i]->value == c) return childs[i];
            }
            return NULL;
        }
        void destroy() {
            for (int i = 0; i < (int)childs.size(); ++i) childs[i]->destroy();
            delete this;
        }
        int getNodeCount() {
            int sum = 1;
            for (int i = 0; i < (int)childs.size(); ++i) sum += childs[i]->getNodeCount();
            return sum;
        }
    };
private:
    void _insert(const char* src, const char *s, int len) {
        Node *n = m_root;
        for (int i = 0; i <= len; ++i) {
            if (i > 0) n->strs.insert(src);
            if (i < len) n = n->getOrCreateChild(s[i]);
        }
    }
    const char* _getStrPtr(const string& src) {
        return m_strPool.insert(src).first->c_str();
    }
private:
    Node *m_root;
    int m_sCount;
    unordered_set<string> m_strPool;
};

int main() {
    Trie t;

    getchar();

    {
        clock_t start = clock();
        ifstream fi("1.txt");
        for (string w; fi >> w; ) {
            int i = 0;
            for (; i < (int)w.size() && !isalnum(w[i]); ++i);
            int j = i;
            for (; j < (int)w.size() && isalnum(w[j]); ++j);
            t.insert(w.substr(i, j - i));
        }
        printf("read %d words, cose %fs, %d nodes\n", t.getSCount(), float(clock() - start) / CLOCKS_PER_SEC, t.getNCount());
    }

    for (string line; getline(cin, line); ) {
        unordered_set<const char*> *res = t.get(line.c_str());
        if (res != NULL) {
            copy(res->begin(), res->end(), ostream_iterator<string>(cout, ","));
        }
        puts("");
    }

    return 0;
}
