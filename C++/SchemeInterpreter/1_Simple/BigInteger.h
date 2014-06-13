#ifndef BIG_INTEGER_H
#define BIG_INTEGER_H

namespace CryptoPP {
    class Integer;
};
typedef CryptoPP::Integer BigIntegerImpl;

class BigInteger {
public:
    BigInteger(int n);
    BigInteger(double d);
    explicit BigInteger(const char *s);

    int toInt() const;
    double toDouble() const;

    bool isNegative() const;

    BigInteger operator + (const BigInteger &o) const;
    BigInteger operator - (const BigInteger &o) const;
    BigInteger operator * (const BigInteger &o) const;
    BigInteger operator / (const BigInteger &o) const;
    BigInteger operator % (const BigInteger &o) const;
    BigInteger powMod(const BigInteger &e, const BigInteger *m = nullptr) const;
    int getLowestBits(int n) const;

    bool operator == (const BigInteger &o) const;
    bool operator != (const BigInteger &o) const;
    bool operator < (const BigInteger &o) const;
    bool operator <= (const BigInteger &o) const;
    bool operator > (const BigInteger &o) const;
    bool operator >= (const BigInteger &o) const;

    void print(ostream &so) const;

private:
    explicit BigInteger(BigIntegerImpl *impl);

private:
    shared_ptr<BigIntegerImpl> mImpl;
};

inline ostream& operator << (ostream &so, const BigInteger &i) {
    i.print(so);
    return so;
}

#endif
