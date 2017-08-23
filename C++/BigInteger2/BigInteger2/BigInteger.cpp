
#include "BigInteger.h"


BigInteger::BigInteger(char const *str, size_t size) {

    std::vector<uint32_t> displayDigits;
    displayDigits.reserve((size + kDisplayBaseDecimals - 1) / kDisplayBaseDecimals);

    for (auto p = str + size; p >= str; p -= kDisplayBaseDecimals) {
        auto s = std::max(p - kDisplayBaseDecimals, str);
        displayDigits.push_back(MultiplePrecisionOp::DecimalsToDisplayDigit(s, p - s));
    }

    mDigits = MultiplePrecisionOp::ChangeBase(move(displayDigits), kDisplayBase, kInternalBase);
}

std::string BigInteger::ToString() const {

    std::vector<uint32_t> displayDigits = MultiplePrecisionOp::ChangeBase(mDigits, kInternalBase, kDisplayBase);

    std::string str(displayDigits.size() * kDisplayBaseDecimals, ' ');
    auto i = MultiplePrecisionOp::DisplayDigitToDecimals(displayDigits.back(), &str[0], kDisplayBaseDecimals, false);
    str.resize(str.size() - (kDisplayBaseDecimals - i));

    for (auto j = displayDigits.size() - 2; i < str.size(); i += kDisplayBaseDecimals, --j) {
        MultiplePrecisionOp::DisplayDigitToDecimals(displayDigits[j], &str[i], kDisplayBaseDecimals, true);
    }

    return str;
}


BigInteger BigInteger::kOne(1);