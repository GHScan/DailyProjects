#ifndef SASSEMBLER_H
#define SASSEMBER_H

#include "SExpression.h"
#include "SValue.h"

class SAssembler {
public:
    SAssembler(const vector<SValue> &constants);

    void assemble(vector<uint8_t> &codes, SExpression e);

private:
    const vector<SValue> &mConstants;
    vector<pair<int, int>> mFrees;
    vector<pair<Atom*, int>> mLabels;
    vector<pair<Atom*, int>> mLabelRefs;
};

#endif
