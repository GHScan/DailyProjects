#include "pch.h"

#include "SParser.h"
#include "SObjectManager.h"
#include "STypes.h"
#include "SPairList.h"

#include <pcrecpp.h>

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
    pcrecpp::RE("\\(").GlobalReplace(" ( ", &s);
    pcrecpp::RE("\\)").GlobalReplace(" ) ", &s);

    for (char *p = (char*)s.c_str(); ; p = nullptr) {
        if (auto token = strtok(p, " \r\n\t")) {
            tokens.push_back(token);
        } else {
            break;
        }
    }
}

static void parseBackward(SObjectManager *mgr, SValue *r, vector<const char *> &tokens) {
    auto token = tokens.back();
    tokens.pop_back();

    if (strcmp(token, "(") == 0) {
        SPairListBuilder builder(mgr, r);

        while (strcmp(tokens.back(), ")")) {
            parseBackward(mgr, builder.push(), tokens);
        }
        tokens.pop_back();

    } else if (strcmp(token, "'") == 0) {
        SPairListBuilder builder(mgr, r);
        mgr->createSymbol(builder.push(), "quote");
        parseBackward(mgr, builder.push(), tokens);

    } else if (re_DOUBLE.FullMatch(token)) {
        mgr->createDouble(r, strtod(token, nullptr));

    } else if (re_INT.FullMatch(token)) {
        long l = strtol(token, nullptr, 10);

        if (errno != ERANGE && l >= SValue::_INT_MIN && l <= SValue::_INT_MAX) {
            mgr->createInt(r, (int)l);
        } else {
            mgr->createBigInt(r, SBigInt::BigInt(token));
        }

    } else if (re_STRING.FullMatch(token)) {
        string s = token;
        mgr->createString(r, unescapeString(s.substr(1, s.size() - 2)).c_str());

    } else {
        mgr->createSymbol(r, token);
    }
}

void parse(SObjectManager *mgr, SValue *r, const string &_s) {
    string s(_s);
    vector<const char *> tokens;
    tokenize(tokens, s);
    reverse(tokens.begin(), tokens.end());

    SPairListBuilder builder(mgr, r);
    while (!tokens.empty()) {
        parseBackward(mgr, builder.push(), tokens);
    }
}
