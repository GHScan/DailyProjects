#ifndef SPARSER_H
#define SPARSER_H

class SObjectManager;
class SValue;

void parse(SObjectManager *mgr, SValue *ret, const string &source);

#endif
