#include "pch.h"

#include <queue>

#include "Utils.h"

struct IMatcher {
    virtual ~IMatcher(){}
    virtual double getMemoryUsage() = 0;
    virtual void setPatternSize(int n) = 0;
    virtual void setPattern(int idx, const char *pat) = 0;
    virtual void find(const char *str, vector<vector<int>> &result) = 0;
};

static const char* simpleStrstr(const char *str, const char *pat) {
    int len = strlen(str);
    int patLen = strlen(pat);
    if (patLen == 0) return str;
    if (len < patLen) return nullptr;

    for (int i = 0; i <= len - patLen; ++i) {
        int j = 0;
        while (j < patLen && str[i + j] == pat[j]) ++j;
        if (j == patLen) return str + i;
    }

    return nullptr;
}

class FuncMatcher: public IMatcher {
public:
    FuncMatcher(const char *(*f)(const char*, const char *)): mF(f) {
    }
    virtual double getMemoryUsage() { return 0; }
    virtual void setPatternSize(int n) {
        mPats.resize(n);
    }
    virtual void setPattern(int idx, const char *pat) {
        mPats[idx] = pat;
    }
    virtual void find(const char *str, vector<vector<int>> &result) {
        for (int idx = 0; idx < (int)mPats.size(); ++idx) {
            const string& pat = mPats[idx];
            for (const char *p = str;;) {
                p = mF(p, pat.c_str());
                if (p == nullptr) break;
                else {
                    result[idx].push_back(p - str);
                    ++p;
                }
            }
        }
    }
private:
    const char *(*mF)(const char *, const char *);
    vector<string> mPats;
};

class CharacterHashMatcher: public IMatcher {
public:
    virtual double getMemoryUsage() {
        return mFreeNodes.size() * sizeof(mFreeNodes[0]);
    }
    virtual void setPatternSize(int n) {
        mFreeNodes.resize(n);
        mFreeNodesIdx = 0;
        mCharactorNodes.resize(128, nullptr);
    }
    virtual void setPattern(int patIdx, const char *pat) {
        Node **p = &mCharactorNodes[pat[0]];
        Node *n = &mFreeNodes[mFreeNodesIdx++];
        n->next = *p;
        n->pat = pat;
        n->patIdx = patIdx;
        *p = n;
    }
    virtual void find(const char *str, vector<vector<int>> &result) {
        int len = strlen(str);
        for (int i = 0; i < len; ++i) {
            assert((unsigned char)str[i] < 128);
            for (Node *n = mCharactorNodes[str[i]]; n != nullptr; n = n->next) {
                if (len - i < (int)n->pat.size()) continue;
                if (memcmp(str + i, n->pat.c_str(), n->pat.size()) == 0) {
                    result[n->patIdx].push_back(i);
                }
            }
        }
    }
private:
    struct Node {
        Node *next;
        string pat;
        int patIdx;
    };
private:
    vector<Node> mFreeNodes;
    int mFreeNodesIdx;
    vector<Node*> mCharactorNodes;
};

class ACAutomationMatcher: public IMatcher {
public:
    virtual double getMemoryUsage() {
        return mNodeMems.size() * BLOCK_SIZE;
    }
    virtual void setPatternSize(int n) {
        mPatSize = n;
        patLens.resize(n, 0);
    }
    virtual void setPattern(int patIdx, const char *pat) {
        patLens[patIdx] = strlen(pat);

        Node *n = mRoot;
        for (; *pat; ++pat) {
            Node **p = n->getChild(*pat);
            if (*p == nullptr) *p = allocNode();
            n = *p;
        }
        assert(n->patIdx == -1);
        n->patIdx = patIdx;

        if (patIdx + 1 == mPatSize) buildPrefix();
    }
    virtual void find(const char *str, vector<vector<int>> &result) {
        Node *n = mRoot;
        for (const char *p = str; *p; ++p) {
            while (n != mRoot && n->getChild(*p)[0] == nullptr) {
                n = n->prefix;
            }
            if (n->getChild(*p)[0] != nullptr) n = n->getChild(*p)[0];

            for (Node *prefix = n; prefix != mRoot; prefix = prefix->prefix) {
                int patIdx = prefix->patIdx;
                if (patIdx != -1) {
                    result[patIdx].push_back((p + 1) - patLens[patIdx] - str);
                }
            }
        }
    }
    ACAutomationMatcher(): mNodeMemOff(0), mPatSize(0), mRoot(allocNode()) {
    }
    ~ACAutomationMatcher() {
        while (!mNodeMems.empty()) {
            free(mNodeMems.back());
            mNodeMems.pop_back();
        }
    }
protected:
    struct Node {
        Node* children[128];
        int patIdx;
        Node *prefix;
        Node** getChild(int c) {
            assert(c < 128);
            return children + c;
        }
        Node(): patIdx(-1), prefix(nullptr) {
            memset(children, 0, sizeof(children));
        }
    };
protected:
    virtual void buildPrefix() {
        queue<Node*> q;
        q.push(mRoot);
        while (!q.empty()) {
            Node *n = q.front();
            q.pop();

            for (int i = 0; i < 128; ++i) {
                Node *c = n->getChild(i)[0];
                if (c == nullptr) continue;
                q.push(c);

                if (n == mRoot) c->prefix = mRoot;
                else {
                    Node *prefix = n->prefix;
                    while (prefix != mRoot && prefix->getChild(i)[0] == nullptr) prefix = prefix->prefix;
                    if (prefix->getChild(i)[0] != nullptr) prefix = prefix->getChild(i)[0];
                    c->prefix = prefix;
                }
            }
        }
    }
protected:
    Node* allocNode() {
        if (mNodeMems.empty() || mNodeMemOff + sizeof(Node) > BLOCK_SIZE) {
            char *p = (char*)malloc(BLOCK_SIZE);
            mNodeMems.push_back(p);
            mNodeMemOff = 0;
        }

        Node *n = new (mNodeMems.back() + mNodeMemOff) Node;
        mNodeMemOff += sizeof(Node);
        return n;
    }
protected:
    vector<char*> mNodeMems;
    int mNodeMemOff;
    int mPatSize;
    Node *mRoot;
    vector<int> patLens;
    static const int BLOCK_SIZE = 32 * 4096;
};

class ACAutomationMatcher2: public ACAutomationMatcher {
public:
    virtual void find(const char *str, vector<vector<int>> &result) {
        Node *n = mRoot;
        for (const char *p = str; *p; ++p) {
            n = n->getChild(*p)[0];
            for (Node *prefix = n; prefix != mRoot; prefix = prefix->prefix) {
                int patIdx = prefix->patIdx;
                if (patIdx != -1) {
                    result[patIdx].push_back(p + 1 - patLens[patIdx] - str);
                }
            }
        }
    }
    virtual void buildPrefix() {
        queue<Node*> q;
        q.push(mRoot);
        while (!q.empty()) {
            Node *n = q.front();
            q.pop();

            for (int i = 0; i < 128; ++i) {
                Node **pc = n->getChild(i);
                if (*pc != nullptr) {
                    q.push(*pc);
                    if (n == mRoot) (*pc)->prefix = mRoot;
                    else (*pc)->prefix = n->prefix->getChild(i)[0];
                    assert((*pc)->prefix != nullptr);
                } else {
                    if (n == mRoot) *pc = mRoot;
                    else *pc = n->prefix->getChild(i)[0];
                    assert(*pc != nullptr);
                }
            }
        }
    }
};

//////////////////////////////
struct MatcherFactory {
    const char *name;
    function<IMatcher*()> factory;
};

static void benchmark(vector<MatcherFactory> &factories) {
    string longStr = checkoutCmd("man bash");
    for (char &c : longStr) {
        if ((unsigned char)c >= 128) c = ' ';
    }

    vector<string> keywords = splitToKeywords(longStr);
    keywords.erase(
            remove_if(keywords.begin(), keywords.end(), [](const string &str){ 
                for (char c : str) if (!isalpha(c)) return true;
                if (str.size() < 3) return true;
                return false;
            }), keywords.end());
    random_shuffle(keywords.begin(), keywords.end());

    vector<vector<int>> result(keywords.size());
    int validResultCount = 0;

    for (auto &factory : factories) {
        auto m = factory.factory();
        m->setPatternSize((int)keywords.size());
        for (int i = 0; i < (int)keywords.size(); ++i) m->setPattern(i, keywords[i].c_str());

        vector<double> times(8);
        for (double &time : times) {
            time = getTime();
            for (int n = 0; n < 1; ++n) {
                for (auto &r : result) r.clear();
                m->find(longStr.c_str(), result);
            }
            time = getTime() - time;
        }
        sort(times.begin(), times.end());

        // check
        int resultCount = 0;
        for (int i = 0; i < (int)keywords.size(); ++i) {
            resultCount += (int)result[i].size();
            const string &kw = keywords[i];
            for (int pos : result[i]) {
                (void)kw; (void)pos;
                assert(memcmp(kw.c_str(), &longStr[pos], kw.size()) == 0);
            }
        }
        if (validResultCount == 0) validResultCount = resultCount;
        else assert(validResultCount == resultCount);

        printf("%s: time=%fs, mem=%fM, result=%d\n", factory.name, accumulate(times.begin(), times.begin() + 3, 0.0) / 3, m->getMemoryUsage() / (1024 * 1024), resultCount);

        delete m;
    }
}

int main() {
    srand(time(nullptr));
    setCpuAffinity(1);

    (void)&simpleStrstr;

#define FACTORY(cmd) { #cmd, []()->IMatcher* { cmd; }}
    vector<MatcherFactory> factories = {
        FACTORY(return new FuncMatcher(strstr)),
        FACTORY(return new FuncMatcher(simpleStrstr)),
        FACTORY(return new CharacterHashMatcher()),
        FACTORY(return new ACAutomationMatcher()),
        FACTORY(return new ACAutomationMatcher2()),
    };
    benchmark(factories);
}
