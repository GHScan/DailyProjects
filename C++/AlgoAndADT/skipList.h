#include "pch.h"

#include <map>
#include <unordered_map>

#include "Utils.h"

template<typename KT, typename VT>
class SkipList {
private:
    struct Node;
    struct Joint {
        int distant;
        Node *next;
    };
    struct Node {
        KT key;
        VT value;
        Joint joints[1];
        Node(const KT &k, const VT &v): key(k), value(v){}
    };
public:
    explicit SkipList(int maxLevel): mMaxLevel(maxLevel), mSize(0) {
        mHead = allocHeadNode();
    }
    SkipList(const SkipList& o): mMaxLevel(o.mMaxLevel), mSize(0) {
        mHead = allocHeadNode();
        for (Node *n = o.mHead; n != nullptr; n = n->joints[0].next) {
            insert(n->key, n->value);
        }
    }
    SkipList& operator = (const SkipList &o) {
        if (this != &o) {
            SkipList tmp(o);
            swap(tmp);
        }
        return *this;
    }
    ~SkipList() {
        for (Node *next; mHead != nullptr; mHead = next) {
            next = mHead->joints[0].next;
            free(mHead);
        }
    }
    void swap(SkipList &o) {
        swap(mHead, o.mHead);
        swap(mSize, o.mSize);
        swap(mMaxLevel, o.mMaxLevel);
    }

    SkipList(SkipList&& o): mMaxLevel(o.mMaxLevel), mSize(0) {
        mHead = allocHeadNode();
        swap(o);
    }
    SkipList& operator = (SkipList &&o) {
        swap(o);
        return *this;
    }

    int size() const {
        return mSize;
    }
    void insert(const KT &k, const VT &v) {
        int distants[32] = {0};
        Node *nexts[32] = {nullptr};

        int level = chooseLevel();
        Node *n = mHead, *p = allocNode(k, v, level);
        for (int i = mMaxLevel - 1; i >= 0; --i) {
            for (Node *next; ; n = next, distants[i] += next->joints[i].distant) {
                next = n->joints[i].next;
                if (next == nullptr || next->key > k) {
                    if (i < level) {
                        p->joints[i].next = next;
                        n->joints[i].next = p;
                    }
                    nexts[i] = next;
                    break;
                } else {
                    assert(next->key != k);
                }
            }
        }

        p->joints[0].distant = 1;
        for (int i = 1; i < mMaxLevel; ++i) {
            Node *next = nexts[i];
            if (i < level) {
                p->joints[i].distant = p->joints[i - 1].distant + distants[i - 1];
                if (next != nullptr) next->joints[i].distant += 1 - p->joints[i].distant;
            } else {
                if (next != nullptr) next->joints[i].distant += 1;
            }
        }

        ++mSize;
    }
    VT* get(const KT &k) {
        Node *n = mHead;
        for (int i = mMaxLevel - 1; i >= 0; --i) {
            for (Node *next;; n = next) {
                next = n->joints[i].next;
                if (next == nullptr || k < next->key) break;
                if (k == next->key) return &next->value;
            }
        }
        return nullptr;
    }
    bool remove(const KT &k) {
        Node *nexts[32] = {nullptr};

        Node *n = mHead, *p = nullptr;
        for (int i = mMaxLevel - 1; i >= 0; --i) {
            for (Node *next;; n = next) {
                next = n->joints[i].next;
                if (next == nullptr || k < next->key) {
                    nexts[i] = next;
                    break;
                }
                if (k == next->key) {
                    p = next;
                    n->joints[i].next = p->joints[i].next;
                    if (Node *pnext = p->joints[i].next) {
                        pnext->joints[i].distant += p->joints[i].distant - 1;
                    }
                    break;
                }
            }
        }
        if (p != nullptr) {
            for (int i = 0; i < 32; ++i) {
                if (nexts[i] != nullptr) nexts[i]->joints[i].distant += -1;
            }

            free(p);
            --mSize;
            return true;
        }
        return false;
    }
    const KT& getKth(int k) const {
        assert(k >= 0 && k < mSize);
        ++k;

        Node *n = mHead;
        for (int i = mMaxLevel - 1; i >= 0; --i) {
            for (Node *next; ; n = next, k -= next->joints[i].distant) {
                if (k == 0) return n->key;
                next = n->joints[i].next;
                if (next == nullptr || k < next->joints[i].distant) break;
            }
        }
        assert(0);
        return mHead->key;
    }

    void dump() {
        printf("skiplist{");
        for (int i = mMaxLevel - 1; i >= 0; --i) {
            printf("[%d]-", mMaxLevel - i);
            for (Node *n = mHead->joints[i].next; n != nullptr; n = n->joints[i].next) {
                printf("(k=%s,dis=%d)", toString(n->key).c_str(), n->joints[i].distant);
            }
            puts("");
        }
        puts("}");
    }
private:
    Node* allocNode(const KT &k, const VT &v, int level) {
        assert(level >= 1);
        return new (malloc(sizeof(Node) + sizeof(Joint) * (level - 1))) Node(k, v);
    }
    Node *allocHeadNode() {
        Node *n = allocNode(KT(), VT(), mMaxLevel);
        for (int i = 0; i < mMaxLevel; ++i) n->joints[i].next = nullptr;
        return n;
    }
    int chooseLevel() const {
        int rint = (rand() << 16) | (rand() & 0xffff);
        for (int level = 1; level < mMaxLevel; ++level) {
            if ((rint >> level) & 1) return level;
        }
        return mMaxLevel;
    }
private:
    Node *mHead;
    int mMaxLevel;
    int mSize;
};

static void correctnessTest() {
    SkipList<string, int> sl(8);
    map<string, int> m;

    for (int i = 0; i < 32 * 1024; ++i) {
        string k = toString(rand() % 1024); 
        int v = rand();
        auto it = m.find(k);
        int *slpv = sl.get(k);
        if (it != m.end()) {
            assert(slpv != nullptr && *slpv == it->second);

            switch (rand() % 2) {
                case 0:
                    m.erase(it);
                    if (!sl.remove(k)) assert(0);
                    assert((int)m.size() == sl.size());
                    break;
                case 1:
                    it->second = v;
                    *slpv = v;
                    break;
                default: assert(0); break;
            }
        } else {
            assert(slpv == nullptr);

            switch (rand() % 2) {
                case 0:
                    if (sl.remove(k)) assert(0);
                    assert((int)m.size() == sl.size());
                    break;
                case 1:
                    m[k] = v;
                    sl.insert(k, v);
                    assert((int)m.size() == sl.size());
                    break;
                default: assert(0); break;
            }
        }
    }

    {
        assert((int)m.size() == sl.size());
        vector<string> keys;
        for (auto &kv : m) keys.push_back(kv.first);
        for (int i = 0; i < (int)keys.size(); ++i) {
            assert(keys[i] == sl.getKth(i));
        }
    }
}
static void benchmark() {
    vector<int> rints(512 * 1024);
    for (int &r : rints) r = rand();
    const int LOOP = 1;
    const int KEY_MOD = 16 * 1024;

#ifdef NDEBUG
    {
        Timer _t("Skiplist d=2");

        for (int n = 0; n < LOOP; ++n) {
            SkipList<int, int> m(2);
            for (int r : rints) {
                int k = r % KEY_MOD; 
                int v = k * 2 + 1;
                switch (r % 4) {
                    case 0: 
                        if (m.get(k) == nullptr) m.insert(k, v);
                        break;
                    case 1:
                        m.remove(k);
                        break;
                    case 2: {
                                int *p = m.get(k);
                                if (p != nullptr) *p = v;
                            }
                            break;
                    case 3: {
                                int *p = m.get(k);
                                if (p != nullptr) assert(*p == v);
                            }
                            break;
                    default: assert(0); break;
                }
            }
        }
    }
#endif

    {
        Timer _t("Skiplist d=9");

        for (int n = 0; n < LOOP; ++n) {
            SkipList<int, int> m(9);
            for (int r : rints) {
                int k = r % KEY_MOD; 
                int v = k * 2 + 1;
                switch (r % 4) {
                    case 0: 
                        if (m.get(k) == nullptr) m.insert(k, v);
                        break;
                    case 1:
                        m.remove(k);
                        break;
                    case 2: {
                                int *p = m.get(k);
                                if (p != nullptr) *p = v;
                            }
                            break;
                    case 3: {
                                int *p = m.get(k);
                                if (p != nullptr) assert(*p == v);
                            }
                            break;
                    default: assert(0); break;
                }
            }
        }
    }

    {
        Timer _t("stl map");

        for (int n = 0; n < LOOP; ++n) {
            map<int, int> m;
            for (int r : rints) {
                int k = r % KEY_MOD; 
                int v = k * 2 + 1;
                switch (r % 4) {
                    case 0: 
                        m[k] = v;
                        break;
                    case 1:
                        m.erase(k);
                        break;
                    case 2: {
                                auto it = m.find(k);
                                if (it != m.end()) it->second = v;
                            }
                            break;
                    case 3: {
                                auto it = m.find(k);
                                if (it != m.end()) assert(it->second == v);
                            }
                            break;
                    default: assert(0); break;
                }
            }
        }
    }

    {
        Timer _t("stl unordered_map");

        for (int n = 0; n < LOOP; ++n) {
            unordered_map<int, int> m;
            for (int r : rints) {
                int k = r % KEY_MOD; 
                int v = k * 2 + 1;
                switch (r % 4) {
                    case 0: 
                        m[k] = v;
                        break;
                    case 1:
                        m.erase(k);
                        break;
                    case 2: {
                                auto it = m.find(k);
                                if (it != m.end()) it->second = v;
                            }
                            break;
                    case 3: {
                                auto it = m.find(k);
                                if (it != m.end()) assert(it->second == v);
                            }
                            break;
                    default: assert(0); break;
                }
            }
        }
    }
}

int main() {
    srand(time(nullptr));
    setCpuAffinity(1);

    correctnessTest();
    benchmark();
}
