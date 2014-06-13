#include "pch.h"
#include "SchemeScanner.h"
#include "SchemeSymbol.h"

SchemeScanner::SchemeScanner(istream &si, SchemeSymbolPool *pool): mStream(si), mSymPool(pool) {
}

const SchemeSymbol* SchemeScanner::nextToken() {
    if (!mPutbackStack.empty()) {
        const SchemeSymbol* sym = mPutbackStack.back();
        mPutbackStack.pop_back();
        return sym;
    }

    char c;
    do {
        if (!mStream.get(c)) return nullptr;
    } while(isspace(c));

    switch (c) {
        case '(': case '[':
            return mSymPool->intern("(");
        case ')': case ']':
            return mSymPool->intern(")");
        case ';':
            while (mStream.get(c) && c != '\n');
            return nextToken();
        case '"': {
                  string s;
                  bool escape = false;

                  for (;;) {
                      if (!mStream.get(c)) break;

                      if (escape) {
                          switch (c) {
                              case 't': s.push_back('\t'); break;
                              case 'r': s.push_back('\r'); break;
                              case 'n': s.push_back('\n'); break;
                              case 'a': s.push_back('\a'); break;
                              case '\\': s.push_back('\\'); break;
                              default: assert(0); break;
                          }
                          escape = false;
                          continue;
                      }

                      if (c == '\\') {
                          escape = true;
                      } else if (c == '"') {
                          break;
                      } else {
                          s.push_back(c);
                      }
                  }

                  return mSymPool->intern(s.c_str(), SchemeSymbol::ID_String);
              }
        case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9': {
                string s(1, c);

                for (;;) {
                    if (!mStream.get(c)) break;

                    if (c != '.' && !isdigit(c)) {
                        mStream.putback(c);
                        break;
                    }

                    s.push_back(c);
                }

                return mSymPool->intern(s.c_str(), SchemeSymbol::ID_Number);
            }
        default: {
            string s(1, c);
            for (;;) {
                if (!mStream.get(c)) break;
                if (isspace(c)) break;
                if (strchr("()[];", c)) {
                    mStream.putback(c);
                    break;
                }
                s.push_back(c);
            }
            return mSymPool->intern(s.c_str());
         }
    }
}
