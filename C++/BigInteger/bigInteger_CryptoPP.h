//
#include "stdafx.h"

#include <time.h>

#include <sstream>

#include <cryptopp/integer.h>
#pragma comment(lib, "F:\\Libraries\\cryptopp561\\lib\\cryptopp.lib")

class BigInteger: public CryptoPP::Integer {
public:
    BigInteger(const CryptoPP::Integer& o): CryptoPP::Integer(o){}
    BigInteger(int i): CryptoPP::Integer(i){}
    int getBitCount() const { return BitCount();}
    BigInteger pow(int n) {
        BigInteger r(1), accum(*this);
        for (; n > 0; n /= 2) {
            if (n & 1) r *= accum;
            accum *= accum;
        }
        return r;
    }
    string toString() const {
        ostringstream so;
        so << *this;
        return so.str();
    }
};
