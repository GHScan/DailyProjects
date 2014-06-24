#ifndef SPARSER_H
#define SPARSER_H

class SObjectManager;
class SValue;

SValue parse(SObjectManager *mgr, const string &source);

#endif
