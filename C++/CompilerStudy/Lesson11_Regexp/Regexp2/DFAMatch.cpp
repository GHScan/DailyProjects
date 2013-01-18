
#include "pch.h"

#include <unordered_map>

#include "RegParser.h"
#include "DFAMatch.h"
#include "DynamicBitset.h"

struct RegexpInfo
{
    RegNodePtr rootNode;
    RegNodePtr acceptNode;
    std::vector<RegNode_Charset*> ID2Node;
    std::unordered_map<RegNode_Charset*, int> node2ID;
    std::vector<DynamicBitset> followNodes;
    std::unordered_map<IRegNode*, bool> nullable;
    std::unordered_map<IRegNode*, DynamicBitset> firstNodes;
    std::unordered_map<IRegNode*, DynamicBitset> lastNodes;
};

class RegNodeVisitor_RegexpInfoBuilder:
    public IRegNodeVisitor
{
public:
    RegNodeVisitor_RegexpInfoBuilder(RegexpInfo& info, RegNodePtr n):
        m_info(info)
    {
        m_info.acceptNode = RegNodePtr(new RegNode_Charset());
        m_info.rootNode = RegNodePtr(new RegNode_Concat(n, m_info.acceptNode));

        m_state = S_SetID; m_info.rootNode->acceptVisitor(this);
        m_info.followNodes.resize(m_info.ID2Node.size(), 
                DynamicBitset((int)m_info.ID2Node.size()));

        m_state = S_SetNullable; m_info.rootNode->acceptVisitor(this);
        m_state = S_SetFirstNodes; m_info.rootNode->acceptVisitor(this);
        m_state = S_SetLastNodes; m_info.rootNode->acceptVisitor(this);
        m_state = S_SetFollowNodes; m_info.rootNode->acceptVisitor(this);
    }
private:
    enum State
    {
        S_SetID,
        S_SetNullable,
        S_SetFirstNodes,
        S_SetLastNodes,
        S_SetFollowNodes,
    };
private:
    virtual void visit(RegNode_Charset *v)
    {
        switch (m_state) {
            case S_SetID: {
                    int id = (int)m_info.ID2Node.size();
                    m_info.ID2Node.push_back(v);
                    m_info.node2ID[v] = id;
                }
                break;
            case S_SetNullable:
                m_info.nullable[v] = false;
                break;
            case S_SetFirstNodes: {
                DynamicBitset sets((int)m_info.ID2Node.size());
                sets.set(m_info.node2ID[v]);
                m_info.firstNodes[v] = sets;
                }
                break;
            case S_SetLastNodes: {
                DynamicBitset sets((int)m_info.ID2Node.size());
                sets.set(m_info.node2ID[v]);
                m_info.lastNodes[v] = sets;
                }
                break;
            default:
                break;
        }
    }
    virtual void visit(RegNode_Capture *v)
    {
        v->node->acceptVisitor(this);
        switch (m_state) {
            case S_SetNullable:
                m_info.nullable[v] = m_info.nullable[v->node.get()];
                break;
            case S_SetFirstNodes:
                m_info.firstNodes[v] = m_info.firstNodes[v->node.get()];
                break;
            case S_SetLastNodes:
                m_info.lastNodes[v] = m_info.lastNodes[v->node.get()];
                break;
            default:
                break;
        }
    }
    virtual void visit(RegNode_Repeat *v)
    {
        v->node->acceptVisitor(this);
        switch (m_state) {
            case S_SetNullable:
                m_info.nullable[v] = v->min == 0;
                break;
            case S_SetFirstNodes: 
                m_info.firstNodes[v] = m_info.firstNodes[v->node.get()];
                break;
            case S_SetLastNodes:
                m_info.lastNodes[v] = m_info.lastNodes[v->node.get()];
                break;
            case S_SetFollowNodes:
                if (v->max > 1) {
                    const DynamicBitset& firstNodes = m_info.firstNodes[v];
                    const DynamicBitset& lastNodes = m_info.lastNodes[v];
                    std::vector<int> ints;
                    lastNodes.toInts(ints);
                    for (int i = 0; i < (int)ints.size(); ++i) {
                        m_info.followNodes[ints[i]].unionWith(firstNodes);
                    }
                }
                break;
            default:
                break;
        }
    }
    virtual void visit(RegNode_Concat *v)
    {
        v->left->acceptVisitor(this);
        v->right->acceptVisitor(this);
        switch (m_state) {
            case S_SetNullable:
                m_info.nullable[v] = m_info.nullable[v->left.get()] && m_info.nullable[v->right.get()];
                break;
            case S_SetFirstNodes: 
                m_info.firstNodes[v] = m_info.firstNodes[v->left.get()];
                if (m_info.nullable[v->left.get()]) {
                    m_info.firstNodes[v].unionWith(m_info.firstNodes[v->right.get()]);
                }
                break;
            case S_SetLastNodes:
                m_info.lastNodes[v] = m_info.lastNodes[v->right.get()];
                if (m_info.nullable[v->right.get()]) {
                    m_info.lastNodes[v].unionWith(m_info.lastNodes[v->left.get()]);
                }
                break;
            case S_SetFollowNodes: {
                    const DynamicBitset& lastNodes = m_info.lastNodes[v->left.get()];
                    const DynamicBitset& firstNodes = m_info.firstNodes[v->right.get()];
                    std::vector<int> ints;
                    lastNodes.toInts(ints);
                    for (int i = 0; i < (int)ints.size(); ++i) {
                        m_info.followNodes[ints[i]].unionWith(firstNodes);
                    }
                }
                break;
            default:
                break;
        }
    }
    virtual void visit(RegNode_Or *v)
    {
        v->left->acceptVisitor(this);
        v->right->acceptVisitor(this);
        switch (m_state) {
            case S_SetNullable:
                m_info.nullable[v] = m_info.nullable[v->left.get()] || m_info.nullable[v->right.get()];
                break;
            case S_SetFirstNodes: 
                m_info.firstNodes[v] = m_info.firstNodes[v->left.get()];
                m_info.firstNodes[v].unionWith(m_info.firstNodes[v->right.get()]);
                break;
            case S_SetLastNodes:
                m_info.lastNodes[v] = m_info.lastNodes[v->left.get()];
                m_info.lastNodes[v].unionWith(m_info.lastNodes[v->right.get()]);
                break;
            case S_SetFollowNodes:
                break;
            default:
                break;
        }
    }
private:
    State m_state;
    RegexpInfo &m_info;
};

static void buildDFAInstance(DFAInstance& ins, RegexpInfo& info)
{
    std::unordered_map<DynamicBitset, int> nodes2State;
    std::vector<DynamicBitset> unmaskedStates;

    unmaskedStates.push_back(info.firstNodes[info.rootNode.get()]);
    nodes2State[unmaskedStates.back()] = ins.newState();
    while (!unmaskedStates.empty()) {
        DynamicBitset sets = unmaskedStates.back();
        unmaskedStates.pop_back();

        int state = nodes2State[sets];
        std::vector<int> ints;
        sets.toInts(ints);
        for (int c = 0; c < 128; ++c) {
            DynamicBitset follow((int)info.ID2Node.size());
            for (int i = 0; i < (int)ints.size(); ++i) {
                if (info.ID2Node[ints[i]]->sets.test((char)c)) {
                    follow.unionWith(info.followNodes[ints[i]]);
                }
            }
            if (follow.any()) {
                int tstate = 0;
                if (nodes2State.count(follow) == 0) {
                    tstate = nodes2State[follow] = ins.newState();
                    unmaskedStates.push_back(follow);
                }
                else tstate = nodes2State[follow];
                ins.setTrans(state, (char)c, tstate);
            }
        }
    }

    DynamicBitset acceptStates(ins.getStateCount());
    int acceptNodeID = info.node2ID[(RegNode_Charset*)info.acceptNode.get()];
    for (std::unordered_map<DynamicBitset, int>::const_iterator iter = nodes2State.begin();
            iter != nodes2State.end();
            ++iter) {
        if (iter->first.test(acceptNodeID)) {
            acceptStates.set(iter->second);
        }
    }
    ins.setAcceptStates(acceptStates);
}

bool compile(DFAInstancePtr &dfa, const RegNodePtr &node)
{
    RegexpInfo info;
    RegNodeVisitor_RegexpInfoBuilder(info, node);
    dfa.reset(new DFAInstance);
    buildDFAInstance(*dfa, info);
    return true;
}
bool match(const DFAInstancePtr &dfa, const std::string& src)
{
    int state = 0;
    for (int i = 0; i < src.size(); ++i) {
        state = dfa->getTrans(state, src[i]);
        if (state == -1) return false;
    }
    return dfa->isAcceptState(state);
}

int DFAInstance::getTrans(int state, char c) const
{
    return m_transMap[(state << 7) + c];
}
bool DFAInstance::isAcceptState(int state) const
{
    return m_acceptStates.test(state);
}
int DFAInstance::getStateCount() const
{
    return (int)m_transMap.size() >> 7;
}
int DFAInstance::newState()
{
    m_transMap.resize(m_transMap.size() + 128, -1);
    return getStateCount() - 1;
}
void DFAInstance::setTrans(int state, char c, int tstate)
{
    m_transMap[(state << 7) + c] = tstate;
}
void DFAInstance::setAcceptStates(const DynamicBitset& sets)
{
    m_acceptStates = sets;
}
void DFAInstance::swap(DFAInstance& o)
{
    m_transMap.swap(o.m_transMap);
    m_acceptStates.swap(o.m_acceptStates);
}

static void swapState(
        int a, int b,
        std::unordered_map<int, DynamicBitset> &new2old, std::unordered_map<int, int> &old2new)
{
    std::vector<int> ints;
    new2old[a].toInts(ints);
    for (int i = 0; i < (int)ints.size(); ++i) old2new[ints[i]] = b;
    ints.clear();
    new2old[b].toInts(ints);
    for (int i = 0; i < (int)ints.size(); ++i) old2new[ints[i]] = a;
    new2old[a].swap(new2old[b]);
}

void DFAInstance::optimize()
{
    std::unordered_map<int, DynamicBitset> new2old;
    std::unordered_map<int, int> old2new;

    DFAInstance newDFA;
    newDFA.newState();
    newDFA.newState();
    new2old[0] = DynamicBitset(getStateCount());
    new2old[1] = DynamicBitset(getStateCount());
    for (int i = 0; i < getStateCount(); ++i) {
        int newState = isAcceptState(i) ? 0 : 1;
        old2new[i] = newState;
        new2old[newState].set(i);
    }
    old2new[-1] = -1;

    for (;;) {
        int oldSize = (int)new2old.size();
        for (std::unordered_map<int, DynamicBitset>::const_iterator iter = new2old.begin();
                iter != new2old.end();
                ++iter) {
            if (iter->second.size() == 1) continue;
            std::vector<int> ints;
            iter->second.toInts(ints);
            std::unordered_map<int, DynamicBitset> _new2old;
            std::unordered_map<int, int> _old2new;
            for (int c = 0; c < 128 && _new2old.size() < 2; ++c) {
                _new2old.clear();
                _old2new.clear();
                for (int i = 0; i < (int)ints.size(); ++i) {
                    int _old = ints[i];
                    int _new = old2new[getTrans(_old, c)];
                    _old2new[_old] = _new;
                    if (_new2old.count(_new) == 0) {
                        _new2old[_new] = DynamicBitset(getStateCount());
                    }
                    _new2old[_new].set(_old);
                }
            }
            if (_new2old.size() > 1) {
                std::vector<int> news;
                news.push_back(iter->first);
                for (int i = 1; i < _new2old.size(); ++i) {
                    news.push_back(newDFA.newState());
                }
                int newIdx = 0;
                for (std::unordered_map<int, DynamicBitset>::const_iterator iter = _new2old.begin();
                        iter != _new2old.end();
                        ++iter, ++newIdx) {
                    new2old[news[newIdx]] = iter->second;
                    std::vector<int> ints;
                    iter->second.toInts(ints);
                    for (int i = 0; i < (int)ints.size(); ++i) {
                        old2new[ints[i]] = news[newIdx];
                    }
                }
            }
        }
        if (oldSize == (int)new2old.size()) break;
    }

    old2new.erase(-1);
    swapState(0, old2new[0], new2old, old2new);
    for (int i = 0; i < newDFA.getStateCount(); ++i) {
        std::vector<int> ints;
        new2old[i].toInts(ints);
        for (int c = 0; c < 128; ++c) {
            int oldt = getTrans(ints[0], c);
            if (oldt == -1) continue;
            newDFA.setTrans(i, c, old2new[oldt]);
        }
    }

    DynamicBitset acceptStates(newDFA.getStateCount());
    for (int i = 0; i < newDFA.getStateCount(); ++i) {
        DynamicBitset& bits = new2old[i];
        std::vector<int> ints;
        bits.toInts(ints);
        for (int j = 0; j < (int)ints.size(); ++j) {
            if (isAcceptState(ints[j])) {
                acceptStates.set(i);
                break;
            }
        }
    }
    newDFA.setAcceptStates(acceptStates);

    swap(newDFA);
    removeDeadState();
}

void DFAInstance::removeDeadState()
{
    DynamicBitset deadStates(getStateCount());
    for (int i = 0; i < getStateCount(); ++i) {
        if (isAcceptState(i)) continue;
        int ts = getTrans(i, 0);
        bool isSame = true;
        for (int c = 1; c < 128; ++c) {
            if (getTrans(i, c) != ts) {
                isSame = false;
                break;
            }
        }
        if (isSame && (ts == -1 || ts == i)) deadStates.set(i);
    }

    DFAInstance newDFA;

    std::vector<int> new2old;
    std::vector<int> old2new(getStateCount(), -1);
    for (int i = 0; i < getStateCount(); ++i) {
        if (!deadStates.test(i)) {
            old2new[i] = newDFA.newState();
            new2old.push_back(i);
        }
    }

    for (int i = 0; i < newDFA.getStateCount(); ++i) {
        for (int c = 0; c < 128; ++c) {
            int ts = getTrans(new2old[i], c);
            if (ts == -1) continue;
            newDFA.setTrans(i, c, old2new[ts]);
        }
    }

    DynamicBitset acceptStates(newDFA.getStateCount());
    for (int i = 0; i < newDFA.getStateCount(); ++i) {
        if (isAcceptState(new2old[i])) acceptStates.set(i);
    }
    newDFA.setAcceptStates(acceptStates);

    swap(newDFA);
}
