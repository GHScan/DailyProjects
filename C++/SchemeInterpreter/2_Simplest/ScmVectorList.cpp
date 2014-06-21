#include "pch.h"
#include "ScmVectorList.h"
#include "ScmTypes.h"
#include "ScmObjectManager.h"

#include <sstream>

#include <pcrecpp.h>
#include <integer.h>

pcrecpp::RE re_LANG("^lang [^\\n]+\\n");
pcrecpp::RE re_COMMENT(";[^\\n]*\\n");
pcrecpp::RE re_DOUBLE("(-)?\\d*\\.\\d+");
pcrecpp::RE re_INT("(-)?\\d+");
pcrecpp::RE re_STRING("\"([^\"]|\\\\.)*\"");

static void tokenize(vector<const char*> &tokens, string &s) {
    re_LANG.GlobalReplace("", &s);
    re_COMMENT.GlobalReplace("", &s);
    pcrecpp::RE("\\[").GlobalReplace("(", &s);
    pcrecpp::RE("\\]").GlobalReplace(")", &s);
    pcrecpp::RE("(").GlobalReplace(" ( ", &s);
    pcrecpp::RE(")").GlobalReplace(" ( ", &s);

    for (char *p = (char*)s.c_str(); ; p = nullptr) {
        tokens.push_back(strtok(p, " "));
    }
}

static void reverseParse(ScmObjectManager *mgr, ScmObject **ret, vector<const char *> &tokens) {
    auto token = tokens.back();
    tokens.pop_back();

    if (strcmp(token, "(") == 0) {
        auto vec = mgr->create<ScmVector>(ret);

        while (strcmp(tokens.back(), ")") != 0) {
            reverseParse(mgr, vec->alloc(), tokens);
        }
        tokens.pop_back();

    } else if (strcmp(token, "'") == 0) {
        auto vec = mgr->create<ScmVector>(ret);

        *vec->alloc() = mgr->getSymbol("quote");
        reverseParse(mgr, vec->alloc(), tokens);

    } else if (re_DOUBLE.FullMatch(token)) {
        mgr->create<ScmDouble>(ret, strtod(token, nullptr));
    } else if (re_INT.FullMatch(token)) {
        long l = strtol(token, nullptr, 10);

        if (errno == ERANGE) {
            mgr->create<ScmBigInt>(ret, ScmBigInt::BigInt(token));
        } else {
            mgr->create<ScmInt>(ret, l);
        }

    } else if (re_STRING.FullMatch(token)) {
        string s = token;
        mgr->create<ScmString>(ret, unescapeString(s.substr(1, s.size() - 2)));
    } else {
        ret[0] = mgr->getSymbol(token);
    }
}

void ScmVectorList::parse(ScmObjectManager *mgr, ScmObject **ret, ScmObject **argEnd) {
    string s = ret[1]->staticCast<ScmString>()->str;
    vector<const char*> tokens;
    tokenize(tokens, s);

    reverse(tokens.begin(), tokens.end());
    reverseParse(mgr, ret, tokens);
}

static void vectorList2PairList(ScmObjectManager *mgr, ScmObject **to, ScmObject **from) {
    if (from[0]->dynamicCast<ScmVector>()) {
        for (auto v : from[0]->dynamicCast<ScmVector>()->getImmutableVec()) {
            to = &mgr->create<ScmPair>(to, v, nullptr)->cdr;
        }

        to[0] = ScmObject::EMPTY;
    } else {
        to[0] = from[0];
    }
}

void ScmVectorList::toPairList(ScmObjectManager *mgr, ScmObject **ret, ScmObject **argEnd) {
    vectorList2PairList(mgr, ret, ret + 1);
}

void ScmVectorList::expandLibraryForms(ScmObjectManager *mgr, ScmObject **ret, ScmObject **argEnd) {
}
