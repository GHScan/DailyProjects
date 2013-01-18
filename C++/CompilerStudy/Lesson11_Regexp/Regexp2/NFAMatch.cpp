#include "pch.h"

#include <set>
#include <unordered_map>

#include "RegParser.h"
#include "NFAMatch.h"

//==================== nfa builder
struct TransiationEntry
{
    NFANodePtr node;
    std::bitset<128> sets;
};
typedef std::vector<NFANodePtr> NFANodeList;
struct NFANode
{
    enum Type
    {
        T_Normal,
        T_Start,
        T_Accept,
    };
    Type type;
    NFANodeList emptyEntries;
    TransiationEntry entry;
    NFANode(Type t): type(t){}
    void resetType() { type = T_Normal;}
};

class RegNodeVisior_NFABuilder:
    public IRegNodeVisitor
{
public:
    NFANodePtr apply(const RegNodePtr &node)
    {
        node->acceptVisitor(this);
        return m_start;
    }
private:
    virtual void visit(RegNode_Charset *v)
    {
        m_start = NFANodePtr(new NFANode(NFANode::T_Start));
        m_accept = NFANodePtr(new NFANode(NFANode::T_Accept));
        m_start->entry.node = m_accept;
        m_start->entry.sets = v->sets;
    }
    virtual void visit(RegNode_Capture *v)
    {
        v->node->acceptVisitor(this);
    }
    virtual void visit(RegNode_Repeat *v)
    {
        // only support ?,*,+
        assert(v->min == 0 || v->min == 1);
        assert(v->max == 1 || v->max == REPEAT_MAX);
        v->node->acceptVisitor(this);
        NFANodePtr n1 = m_start, n2 = m_accept;
        m_start = NFANodePtr(new NFANode(NFANode::T_Start));
        m_accept = NFANodePtr(new NFANode(NFANode::T_Accept));
        n1->resetType(); n2->resetType();
        m_start->emptyEntries.push_back(n1);
        n2->emptyEntries.push_back(m_accept);
        if (v->min == 0) {
            m_start->emptyEntries.push_back(m_accept);
        }
        if (v->max == REPEAT_MAX) {
            n2->emptyEntries.push_back(n1);
        }
    }
    virtual void visit(RegNode_Concat *v)
    {
        v->left->acceptVisitor(this);
        NFANodePtr n1 = m_start, n2 = m_accept;
        v->right->acceptVisitor(this);
        NFANodePtr n3 = m_start, n4 = m_accept;
        m_start = n1, m_accept = n4;
        *n2 = *n3;
        n2->resetType();
    }
    virtual void visit(RegNode_Or *v)
    {
        v->left->acceptVisitor(this);
        NFANodePtr n1 = m_start, n2 = m_accept;
        v->right->acceptVisitor(this);
        NFANodePtr n3 = m_start, n4 = m_accept;
        m_start = NFANodePtr(new NFANode(NFANode::T_Start));
        m_accept = NFANodePtr(new NFANode(NFANode::T_Accept));
        n1->resetType(); n2->resetType();
        n3->resetType(); n4->resetType();
        m_start->emptyEntries.push_back(n1);
        m_start->emptyEntries.push_back(n3);
        n2->emptyEntries.push_back(m_accept);
        n4->emptyEntries.push_back(m_accept);
    }
private:
    NFANodePtr m_start, m_accept;
};

typedef std::set<NFANode*> NFANodeSet;
class NFARuntime
{
public:
    NFARuntime(const NFANodePtr& start):
        m_setIdx(0)
    {
        insertNode(m_sets[m_setIdx], start.get());
    }
    bool transit(char c)
    {
        int newIdx = 1 - m_setIdx;
        NFANodeSet &oldSet = m_sets[m_setIdx], &newSet = m_sets[newIdx];
        m_setIdx = newIdx;
        newSet.clear();
        for (NFANodeSet::const_iterator iter = oldSet.begin(); iter != oldSet.end(); ++iter) {
            NFANode *node = *iter;
            if (node->entry.sets.test(c)) {
                insertNode(newSet, node->entry.node.get());
            }
        }
        return !newSet.empty();
    }
    bool isAccept() const
    {
        const NFANodeSet &set = m_sets[m_setIdx];
        for (NFANodeSet::const_iterator iter = set.begin(); iter != set.end(); ++iter) {
            if ((*iter)->type == NFANode::T_Accept) return true;
        }
        return false;
    }
private:
    void insertNode(NFANodeSet& sets, NFANode *n)
    {
        if (sets.insert(n).second) {
            for (NFANodeList::const_iterator iter = n->emptyEntries.begin(); iter != n->emptyEntries.end(); ++iter) {
                insertNode(sets, iter->get());
            }
        }
    }
private:
    int m_setIdx;
    NFANodeSet m_sets[2];
};

bool compile(NFANodePtr &nfa, const RegNodePtr &node)
{
    nfa = RegNodeVisior_NFABuilder().apply(node);
    return nfa != NULL;
}

bool match(const NFANodePtr &nfa, const std::string& src)
{
    NFARuntime r(nfa);
    for (int i = 0; i < src.size(); ++i) if (!r.transit(src[i])) break;
    return r.isAccept();
}

class NFA2DFAConverter
{
public:
    NFA2DFAConverter(DFAInstance& dfa, NFANodePtr& nfa):
        m_dfa(dfa)
    {
        setIDForNodes(nfa.get());

        std::unordered_map<DynamicBitset, int> bits2State;
        std::vector<DynamicBitset> unmaskedBites;

        NFANodeSet nodes;
        getEquativeNodes(nodes, nfa.get());
        unmaskedBites.push_back(nodes2Bits(nodes));
        bits2State[unmaskedBites.back()] = dfa.newState();
        while (!unmaskedBites.empty()) {
            DynamicBitset bits = unmaskedBites.back();
            unmaskedBites.pop_back();
            int state = bits2State[bits];
            std::vector<int> ints;
            bits.toInts(ints);

            for (int c = 0; c < 128; ++c) {
                nodes.clear();
                for (int i = 0; i < (int)ints.size(); ++i) {
                    NFANode *n = m_ID2Node[ints[i]];
                    if (n->entry.sets.test(c)) {
                        getEquativeNodes(nodes, n->entry.node.get());
                    }
                }
                DynamicBitset dbits(nodes2Bits(nodes));
                int dstate = 0;
                if (bits2State.count(dbits) == 0) {
                    dstate = bits2State[dbits] = dfa.newState();
                    unmaskedBites.push_back(dbits);
                } 
                else dstate = bits2State[dbits];
                dfa.setTrans(state, c, dstate);
            }
        }

        DynamicBitset acceptStates(dfa.getStateCount());
        for (std::unordered_map<DynamicBitset, int>::const_iterator iter = bits2State.begin();
                iter != bits2State.end();
                ++iter) {
            bool accept = false;
            std::vector<int> ints;
            iter->first.toInts(ints);
            for (int i = 0; i < (int)ints.size(); ++i) {
                NFANode* p = m_ID2Node[ints[i]];
                if (p->type == NFANode::T_Accept) {
                    accept = true;
                    break;
                }
            }
            if (accept) {
                acceptStates.set(iter->second);
            }
        }
        dfa.setAcceptStates(acceptStates);
    }
private:
    void setIDForNodes(NFANode* n)
    {
        if (n == NULL) return;
        if (m_node2ID.count(n) > 0) return;
        m_node2ID[n] = (int)m_ID2Node.size();
        m_ID2Node.push_back(n);
        setIDForNodes(n->entry.node.get());
        for (NFANodeList::const_iterator iter = n->emptyEntries.begin();
                iter != n->emptyEntries.end();
                ++iter) {
            setIDForNodes(iter->get());
        }
    }
    void getEquativeNodes(NFANodeSet& s, NFANode* n)
    {
        if (s.insert(n).second) {
            for (NFANodeList::const_iterator iter = n->emptyEntries.begin();
                    iter != n->emptyEntries.end();
                    ++iter) {
                getEquativeNodes(s, iter->get());
            }
        }
    }
    DynamicBitset nodes2Bits(NFANodeSet& s)
    {
        DynamicBitset bits((int)m_ID2Node.size());
        for (NFANodeSet::const_iterator iter = s.begin();
                iter != s.end(); 
                ++iter) {
            bits.set(m_node2ID[*iter]);
        }
        return bits;
    }
private:
    std::unordered_map<NFANode*, int> m_node2ID;
    std::vector<NFANode*> m_ID2Node;
    DFAInstance& m_dfa;
};

bool convertToDFA(DFAInstancePtr& dfa, NFANodePtr& nfa)
{
    dfa.reset(new DFAInstance());
    NFA2DFAConverter(*dfa, nfa);
    return true;
}
