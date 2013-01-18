
#ifndef DFAMATCH_H
#define DFAMATCH_H

#include <memory>

#include "DynamicBitset.h"

class DFAInstance
{
public:
    int getTrans(int state, char c) const;
    bool isAcceptState(int state) const;
    int getStateCount() const;
public:
    int newState();
    void setTrans(int state, char c, int tstate);
    void setAcceptStates(const DynamicBitset& sets);
    void optimize();
private:
    void swap(DFAInstance& o);
    void removeDeadState();
private:
    std::vector<int> m_transMap;
    DynamicBitset m_acceptStates;
};
typedef std::shared_ptr<DFAInstance> DFAInstancePtr;

bool compile(DFAInstancePtr &dfa, const RegNodePtr &node);
bool match(const DFAInstancePtr &dfa, const std::string& src);

#endif
