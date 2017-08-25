#vim:fileencoding=utf-8

#----------------------------------------
def genHeader():
    return '''
    '''

#----------------------------------------
def maxDLP(floatType):
    return 8 if floatType == 'float' else 4 # for avx
def genLshift(var, bits):
    return '(%s << %d)' % (var, bits) if bits > 0 else var
def genRshift(var, bits):
    return '(%s >> %d)' % (var, bits) if bits > 0 else var


def genReversedBitsExp(varName, bits, reversedBits):

    parts = ['(%s >> %d)' % (varName, reversedBits)]
    for i in range(0, reversedBits, 8):
        if i + 8 <= reversedBits:
            part = genLshift('Reverse8Bits(%s & 0xff)' % genRshift(varName, i), bits - 8 - i)
        else:
            currBits = reversedBits - i
            currMask = (1 << currBits) - 1
            rshift = (8 - currBits) - (bits - currBits - i)
            if rshift >= 0:
                part = genRshift('Reverse8Bits(%s & %s)' % (genRshift(varName, i), hex(currMask)), rshift)
            else:
                part = genLshift('Reverse8Bits(%s & %s)' % (genRshift(varName, i), hex(currMask)), -rshift)
        parts.append(part)

    return ' | '.join(parts)


#----------------------------------------
def genBitReverseCopy_SingleParallel2(bits):
    size = 1 << bits
    halfSize = size >> 1
    dlp = maxDLP('float')
    body = '''
void BitReverseCopy_%d(float *destReals, float *destImags, float const *srcReals, float const *srcImags) {
    for (size_t i = 0; i < %d; i += %d) {
        SplitEvenOdd_%dPS(destReals, i / 2, i / 2 + %d, srcReals, i, i + %d);
        SplitEvenOdd_%dPS(destImags, i / 2, i / 2 + %d, srcImags, i, i + %d);
    }
}
''' % (bits, size, 2 * dlp, dlp, halfSize, dlp, dlp, halfSize, dlp)
    return body

def genIBitReverseCopy_SingleParallel2(bits):
    size = 1 << bits
    halfSize = size >> 1
    dlp = maxDLP('float')
    body = '''
void IBitReverseCopy_%d(float *destReals, float *destImags, float const *srcReals, float const *srcImags) {
    auto scale = CreateScaler_%dPS(%.20ff);
    for (size_t i = 0; i < %d; i += %d) {
        SplitEvenOdd_Scaled%dPS(destReals, i / 2, i / 2 + %d, srcReals, i, i + %d, scale);
        SplitEvenOdd_Scaled%dPS(destImags, i / 2, i / 2 + %d, srcImags, i, i + %d, scale);
    }
}
''' % (bits, dlp, 1 / size, size, 2 * dlp, dlp, halfSize, dlp, dlp, halfSize, dlp)
    return body

def genBitReverseCopy_SingleParallel4(bits):
    size = 1 << bits
    halfSize = size >> 1
    quarterSize = halfSize >> 1
    dlp = maxDLP('float')
    body = '''
void BitReverseCopy_%d(float *destReals, float *destImags, float const *srcReals, float const *srcImags) {
    for (size_t i = 0; i < %d; i += %d) {
        SplitMod4_%dPS(destReals, i / 4, i / 4 + %d, i / 4 + %d, i / 4 + %d, srcReals, i, i + %d, i + %d, i + %d);
        SplitMod4_%dPS(destImags, i / 4, i / 4 + %d, i / 4 + %d, i / 4 + %d, srcImags, i, i + %d, i + %d, i + %d);
    }
}
''' % (bits, size, 4 * dlp, 
       dlp, quarterSize, halfSize, halfSize + quarterSize, dlp, 2 * dlp, 3 * dlp,
       dlp, quarterSize, halfSize, halfSize + quarterSize, dlp, 2 * dlp, 3 * dlp)
    return body

def genIBitReverseCopy_SingleParallel4(bits):
    size = 1 << bits
    halfSize = size >> 1
    quarterSize = halfSize >> 1
    dlp = maxDLP('float')
    body = '''
void IBitReverseCopy_%d(float *destReals, float *destImags, float const *srcReals, float const *srcImags) {
    auto scale = CreateScaler_%dPS(%.20ff);
    for (size_t i = 0; i < %d; i += %d) {
        SplitMod4_Scaled%dPS(destReals, i / 4, i / 4 + %d, i / 4 + %d, i / 4 + %d, srcReals, i, i + %d, i + %d, i + %d, scale);
        SplitMod4_Scaled%dPS(destImags, i / 4, i / 4 + %d, i / 4 + %d, i / 4 + %d, srcImags, i, i + %d, i + %d, i + %d, scale);
    }
}
''' % (bits, dlp, 1 / size, size, 4 * dlp,
       dlp, quarterSize, halfSize, halfSize + quarterSize, dlp, 2 * dlp, 3 * dlp,
       dlp, quarterSize, halfSize, halfSize + quarterSize, dlp, 2 * dlp, 3 * dlp)
    return body

def genBitReverseCopy_SingleParallel8(bits, reversedBits):
    size = 1 << bits
    span = 1 << reversedBits
    body = '''
void BitReverseCopy_%d(float *destReals, float *destImags, float const *srcReals, float const *srcImags) {
    constexpr size_t span = %d;
    for (size_t off = 0; off < span; off += 8) {
        for (size_t i = off; i < %d; i += 8 * span) {
            auto i0 = i + 0, i1 = i + 1, i2 = i + 2, i3 = i + 3, i4 = i + 4, i5 = i + 5, i6 = i + 6, i7 = i + 7;
            auto revI0 = %s;
            auto revI1 = %s;
            auto revI2 = %s;
            auto revI3 = %s;
            auto revI4 = %s;
            auto revI5 = %s;
            auto revI6 = %s;
            auto revI7 = %s;
            Transpose_8PS(
                destReals, revI0, revI1, revI2, revI3, revI4, revI5, revI6, revI7,
                srcReals, i, i + span, i + 2 * span, i + 3 * span, 
                          i + 4 * span, i + 5 * span, i + 6 * span, i + 7 * span);
            Transpose_8PS(
                destImags, revI0, revI1, revI2, revI3, revI4, revI5, revI6, revI7,
                srcImags, i, i + span, i + 2 * span, i + 3 * span,
                i + 4 * span, i + 5 * span, i + 6 * span, i + 7 * span);
        }
    }
}
''' % (bits, span, size, 
       genReversedBitsExp('i0', bits, reversedBits), 
       genReversedBitsExp('i1', bits, reversedBits), 
       genReversedBitsExp('i2', bits, reversedBits), 
       genReversedBitsExp('i3', bits, reversedBits),
       genReversedBitsExp('i4', bits, reversedBits), 
       genReversedBitsExp('i5', bits, reversedBits), 
       genReversedBitsExp('i6', bits, reversedBits), 
       genReversedBitsExp('i7', bits, reversedBits))
    return body

def genIBitReverseCopy_SingleParallel8(bits, reversedBits):
    size = 1 << bits
    span = 1 << reversedBits
    body = '''
void IBitReverseCopy_%d(float *destReals, float *destImags, float const *srcReals, float const *srcImags) {
    auto scale = CreateScaler_8PS(%.20ff);
    constexpr size_t span = %d;
    for (size_t off = 0; off < span; off += 8) {
        for (size_t i = off; i < %d; i += 8 * span) {
            auto i0 = i + 0, i1 = i + 1, i2 = i + 2, i3 = i + 3, i4 = i + 4, i5 = i + 5, i6 = i + 6, i7 = i + 7;
            auto revI0 = %s;
            auto revI1 = %s;
            auto revI2 = %s;
            auto revI3 = %s;
            auto revI4 = %s;
            auto revI5 = %s;
            auto revI6 = %s;
            auto revI7 = %s;
            Transpose_Scaled8PS(
                destReals, revI0, revI1, revI2, revI3, revI4, revI5, revI6, revI7,
                srcReals, i, i + span, i + 2 * span, i + 3 * span, 
                          i + 4 * span, i + 5 * span, i + 6 * span, i + 7 * span, scale);
            Transpose_Scaled8PS(
                destImags, revI0, revI1, revI2, revI3, revI4, revI5, revI6, revI7,
                srcImags, i, i + span, i + 2 * span, i + 3 * span,
                i + 4 * span, i + 5 * span, i + 6 * span, i + 7 * span, scale);
        }
    }
}
''' % (bits, 1 / size, span, size, 
       genReversedBitsExp('i0', bits, reversedBits), 
       genReversedBitsExp('i1', bits, reversedBits), 
       genReversedBitsExp('i2', bits, reversedBits), 
       genReversedBitsExp('i3', bits, reversedBits),
       genReversedBitsExp('i4', bits, reversedBits), 
       genReversedBitsExp('i5', bits, reversedBits), 
       genReversedBitsExp('i6', bits, reversedBits), 
       genReversedBitsExp('i7', bits, reversedBits))
    return body

#----------------------------------------
def genBitReverseCopy_DoubleParallel2(bits):
    size = 1 << bits
    halfSize = size >> 1
    dlp = maxDLP('double')
    body = '''
void BitReverseCopy_%d(double *destReals, double *destImags, double const *srcReals, double const *srcImags) {
    for (size_t i = 0; i < %d; i += %d) {
        SplitEvenOdd_%dPD(destReals, i / 2, i / 2 + %d, srcReals, i, i + %d);
        SplitEvenOdd_%dPD(destImags, i / 2, i / 2 + %d, srcImags, i, i + %d);
    }
}
''' % (bits, size, 2 * dlp, dlp, halfSize, dlp, dlp, halfSize, dlp)
    return body

def genIBitReverseCopy_DoubleParallel2(bits):
    size = 1 << bits
    halfSize = size >> 1
    dlp = maxDLP('double')
    body = '''
void IBitReverseCopy_%d(double *destReals, double *destImags, double const *srcReals, double const *srcImags) {
    auto scale = CreateScaler_%dPD(%.20f);
    for (size_t i = 0; i < %d; i += %d) {
        SplitEvenOdd_Scaled%dPD(destReals, i / 2, i / 2 + %d, srcReals, i, i + %d, scale);
        SplitEvenOdd_Scaled%dPD(destImags, i / 2, i / 2 + %d, srcImags, i, i + %d, scale);
    }
}
''' % (bits, dlp, 1 / size, size, 2 * dlp, dlp, halfSize, dlp, dlp, halfSize, dlp)
    return body

def genBitReverseCopy_DoubleParallel4(bits, reversedBits):
    size = 1 << bits
    span = 1 << reversedBits
    body = '''
void BitReverseCopy_%d(double *destReals, double *destImags, double const *srcReals, double const *srcImags) {
    constexpr size_t span = %d;
    for (size_t off = 0; off < span; off += 4) {
        for (size_t i = off; i < %d; i += 4 * span) {
            auto i0 = i + 0, i1 = i + 1, i2 = i + 2, i3 = i + 3;
            auto revI0 = %s;
            auto revI1 = %s;
            auto revI2 = %s;
            auto revI3 = %s;
            Transpose_4PD(destReals, revI0, revI1, revI2, revI3, srcReals, i, i + span, i + 2 * span, i + 3 * span);
            Transpose_4PD(destImags, revI0, revI1, revI2, revI3, srcImags, i, i + span, i + 2 * span, i + 3 * span);
        }
    }
}
''' % (bits, span, size, 
       genReversedBitsExp('i0', bits, reversedBits), 
       genReversedBitsExp('i1', bits, reversedBits), 
       genReversedBitsExp('i2', bits, reversedBits), 
       genReversedBitsExp('i3', bits, reversedBits))
    return body

def genIBitReverseCopy_DoubleParallel4(bits, reversedBits):
    size = 1 << bits
    span = 1 << reversedBits
    body = '''
void IBitReverseCopy_%d(double *destReals, double *destImags, double const *srcReals, double const *srcImags) {
    auto scale = CreateScaler_4PD(%.20f);
    constexpr size_t span = %d;
    for (size_t off = 0; off < span; off += 4) {
        for (size_t i = off; i < %d; i += 4 * span) {
            auto i0 = i + 0, i1 = i + 1, i2 = i + 2, i3 = i + 3;
            auto revI0 = %s;
            auto revI1 = %s;
            auto revI2 = %s;
            auto revI3 = %s;
            Transpose_Scaled4PD(destReals, revI0, revI1, revI2, revI3, srcReals, i, i + span, i + 2 * span, i + 3 * span, scale);
            Transpose_Scaled4PD(destImags, revI0, revI1, revI2, revI3, srcImags, i, i + span, i + 2 * span, i + 3 * span, scale);
        }
    }
}
''' % (bits, 1 / size, span, size, 
       genReversedBitsExp('i0', bits, reversedBits), 
       genReversedBitsExp('i1', bits, reversedBits), 
       genReversedBitsExp('i2', bits, reversedBits), 
       genReversedBitsExp('i3', bits, reversedBits))
    return body


#----------------------------------------
def genBitReverseCopy(floatType, bits, remainingBits, direction):
    reversedBits = bits - remainingBits
    if floatType == 'float':
        if reversedBits == 1:
            return genBitReverseCopy_SingleParallel2(bits) if direction > 0 else genIBitReverseCopy_SingleParallel2(bits)
        elif reversedBits == 2:
            return genBitReverseCopy_SingleParallel4(bits) if direction > 0 else genIBitReverseCopy_SingleParallel4(bits)
        else:
            return genBitReverseCopy_SingleParallel8(bits, reversedBits) if direction > 0 else genIBitReverseCopy_SingleParallel8(bits, reversedBits)
    else:
        if reversedBits == 1:
            return genBitReverseCopy_DoubleParallel2(bits) if direction > 0 else genIBitReverseCopy_DoubleParallel2(bits)
        else:
            return genBitReverseCopy_DoubleParallel4(bits, reversedBits) if direction > 0 else genIBitReverseCopy_DoubleParallel4(bits, reversedBits)


def genBitReverseCopyFunctions(path, floatType, unrolledBits, maxBits):
    lines = [genHeader()]
    for bits in range(unrolledBits + 1, maxBits + 1):
        lines.append(genBitReverseCopy(floatType, bits, unrolledBits, 1))
        lines.append(genBitReverseCopy(floatType, bits, unrolledBits, -1))
    open(path, 'w').write('\n'.join(lines))


#----------------------------------------
if __name__ == '__main__':
    unrolledBits = 8
    maxBits = 24
    outputDir = r'..\ScanFFT\include\ScanFFTImpl\Generated'
    
    genBitReverseCopyFunctions(outputDir + r'\BitReverseCopy_Double.h', 'double', unrolledBits, maxBits)
    genBitReverseCopyFunctions(outputDir + r'\BitReverseCopy_Single.h', 'float', unrolledBits, maxBits)

