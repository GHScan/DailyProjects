
#ifndef SHA224_H
#define SHA224_H

/*
ref: http://en.wikipedia.org/wiki/SHA-2

SHA-224 is identical to SHA-256, except that:

    @ the initial hash values h0 through h7 are different, and
    @ the output is constructed by omitting h7.

    SHA-224 initial hash values (in big endian):
        (The second 32 bits of the fractional parts of the square roots of the 9th through 16th primes 23..53)
         h[0..7] :=
             0xc1059ed8, 0x367cd507, 0x3070dd17, 0xf70e5939, 0xffc00b31, 0x68581511, 0x64f98fa7, 0xbefa4fa4
*/

#endif
