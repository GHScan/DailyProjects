
#ifndef NFAMATCH_H
#define NFAMATCH_H

#include "DFAMatch.h"

struct NFANode;
typedef std::shared_ptr<NFANode> NFANodePtr;

bool compile(NFANodePtr &nfa, const RegNodePtr &node);
bool match(const NFANodePtr &nfa, const std::string& src);
bool convertToDFA(DFAInstancePtr& dfa, NFANodePtr& nfa);

#endif
