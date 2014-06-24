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

static SValue parseBackward(SObjectManager *mgr, vector<const char *> &tokens) {
    auto token = tokens.back();
    tokens.pop_back();

    if (strcmp(token, "(") == 0) {
        SPairListBuilder builder(mgr);

        while (strcmp(tokens.back(), ")")) {
            builder.push(parseBackward(mgr, tokens));
        }
        tokens.pop_back();

        return builder.getList();

    } else if (strcmp(token, "'") == 0) {
        SPairListBuilder builder(mgr);
        builder
            .push(SValue(mgr->createSymbol("quote")))
            .push(parseBackward(mgr, tokens));
        return builder.getList();

    } else if (re_DOUBLE.FullMatch(token)) {
        return SValue(mgr->createDouble(strtod(token, nullptr)));

    } else if (re_INT.FullMatch(token)) {
        long l = strtol(token, nullptr, 10);

        if (errno != ERANGE && l >= SValue::_INT_MIN && l <= SValue::_INT_MAX) {
            return SValue((int)l);
        } else {
            return SValue(mgr->createBigInt(SBigInt::BigInt(token)));
        }

    } else if (re_STRING.FullMatch(token)) {
        string s = token;
        return SValue(mgr->createString(unescapeString(s.substr(1, s.size() - 2)).c_str()));

    } else {
        return SValue(mgr->createSymbol(token));
    }
}

SValue parse(SObjectManager *mgr, const string &source) {
    string s(source);
    vector<const char *> tokens;
    tokenize(tokens, s);
    reverse(tokens.begin(), tokens.end());

    SPairListBuilder builder(mgr);
    while (!tokens.empty()) {
        builder.push(parseBackward(mgr, tokens));
    }
    return builder.getList();
}
