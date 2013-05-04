
#include "pch.h"

#include <math.h>

#include "LuaLibs.h"

static const NumberType PI = 3.1415926535;
static const NumberType E = 2.71828182846;

static void math_abs(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    NumberType n = args[0].getNumber();
    rets.push_back(LuaValue(n < 0 ? -n : n));
}
static void math_acos(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    NumberType n = args[0].getNumber();
    rets.push_back(LuaValue(NumberType(::acos(n))));
}
static void math_asin(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    NumberType n = args[0].getNumber();
    rets.push_back(LuaValue(NumberType(::asin(n))));
}
static void math_atan(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    NumberType n = args[0].getNumber();
    rets.push_back(LuaValue(NumberType(::atan(n))));
}
static void math_atan2(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    NumberType y = args[0].getNumber(), x = args[1].getNumber();
    rets.push_back(LuaValue(NumberType(::atan2(y, x))));
}
static void math_ceil(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    NumberType n = args[0].getNumber();
    rets.push_back(LuaValue(NumberType(::ceil(n))));
}
static void math_cos(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    NumberType n = args[0].getNumber();
    rets.push_back(LuaValue(NumberType(::cos(n))));
}
static void math_cosh(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    NumberType n = args[0].getNumber();
    rets.push_back(LuaValue(NumberType(::cosh(n))));
}
static void math_deg(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    NumberType n = args[0].getNumber();
    rets.push_back(LuaValue(NumberType(NumberType(n * 180 / PI))));
}
static void math_exp(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    NumberType n = args[0].getNumber();
    rets.push_back(LuaValue(NumberType(NumberType(::pow(E, n)))));
}
static void math_floor(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    NumberType n = args[0].getNumber();
    rets.push_back(LuaValue(NumberType(::floor(n))));
}
static void math_fmod(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    NumberType x = args[0].getNumber(), y = args[1].getNumber();
    rets.push_back(LuaValue(NumberType(::fmod(x, y))));
}
static void math_frexp(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    ASSERT(0);
}
static void math_ldexp(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    ASSERT(0);
}
static void math_log(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    NumberType n = args[0].getNumber();
    rets.push_back(LuaValue(NumberType(::log(n))));
}
static void math_log10(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    NumberType n = args[0].getNumber();
    rets.push_back(LuaValue(NumberType(::log(n) / ::log(10))));
}
static void math_max(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    LuaValue v(args[0]);
    for (int i = 1; i < (int)args.size(); ++i) {
        if (args[i] > v) v = args[i];
    }
    rets.push_back(v);
}
static void math_min(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    LuaValue v(args[0]);
    for (int i = 1; i < (int)args.size(); ++i) {
        if (args[i] < v) v = args[i];
    }
    rets.push_back(v);
}
static void math_modf(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    NumberType n = args[0].getNumber();
    double intral;
    double fac = ::modf(n, &intral);
    rets.push_back(LuaValue(NumberType(intral))); rets.push_back(LuaValue(NumberType(fac)));
}
static void math_pow(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    NumberType x = args[0].getNumber(), y = args[1].getNumber();
    rets.push_back(LuaValue(NumberType(::pow(x, y))));
}
static void math_rad(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    NumberType n = args[0].getNumber();
    rets.push_back(LuaValue(NumberType(NumberType(n * PI / 180))));
}
static void math_random(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    if (args.empty()) {
        rets.push_back(LuaValue(rand() / (NumberType(RAND_MAX) + 1)));
    } else if (args.size() == 1) {
        NumberType n = args[0].getNumber();
        ASSERT(n > 1);
        rets.push_back(LuaValue(1 + rand() * (n - 1) / RAND_MAX));
    } else if (args.size() >= 2) {
        NumberType m = args[0].getNumber(), n = args[1].getNumber();
        ASSERT(n > m);
        rets.push_back(LuaValue(m + rand() * (n - m) / RAND_MAX));
    } else ;
}
static void math_randomseed(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    NumberType n = args[0].getNumber();
    srand((unsigned int)n);
}
static void math_sin(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    NumberType x = args[0].getNumber();
    rets.push_back(LuaValue(NumberType(::sin(x))));
}
static void math_sinh(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    NumberType x = args[0].getNumber();
    rets.push_back(LuaValue(NumberType(::sinh(x))));
}
static void math_sqrt(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    NumberType x = args[0].getNumber();
    rets.push_back(LuaValue(NumberType(::sqrt(x))));
}
static void math_tan(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    NumberType x = args[0].getNumber();
    rets.push_back(LuaValue(NumberType(::tan(x))));
}
static void math_tanh(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    NumberType x = args[0].getNumber();
    rets.push_back(LuaValue(NumberType(::tanh(x))));
}

extern void openLib_math() {
    auto table = LuaTable::create();
    LuaVM::instance()->getGlobalTable()->set(LuaValue("math"), LuaValue(table));

#define ENTRY(name) {#name, &math_##name}
    CFuncEntry entries[] = {
        ENTRY(abs), ENTRY(acos), ENTRY(asin),
        ENTRY(atan), ENTRY(atan2), ENTRY(ceil),
        ENTRY(cos), ENTRY(cosh), ENTRY(deg),
        ENTRY(exp), ENTRY(floor), ENTRY(fmod),
        ENTRY(frexp), ENTRY(ldexp), ENTRY(log),
        ENTRY(log10), ENTRY(max), ENTRY(min),
        ENTRY(modf), ENTRY(pow), ENTRY(rad),
        ENTRY(random), ENTRY(randomseed), ENTRY(sin),
        ENTRY(sinh), ENTRY(sqrt), ENTRY(tan),
        ENTRY(tanh),
    };
#undef ENTRY
    for (auto &entry : entries) table->set(LuaValue(entry.name), LuaValue(CFunction::create(entry.func)));

    table->set(LuaValue("huge"), LuaValue(NumberType(HUGE_VAL)));
    table->set(LuaValue("pi"), LuaValue(NumberType(PI)));
}
