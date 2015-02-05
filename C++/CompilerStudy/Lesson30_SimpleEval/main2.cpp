#include "pch.h" 

static void whitespace(const char *&input) {
	for (; isspace(*input); ++input);
}

static bool term(const char *&input, const char *s) {
	whitespace(input);
	for (; *s && *s == *input; ++input, ++s);
	return *s == 0;
}

static double number(const char *&input) {
	whitespace(input);

	double d = 0;
	for (; isdigit(*input); ++input) {
		d = d * 10 + (*input - '0');
	}

	if (*input == '.') {
		++input;

		double factor = 1;
		for (; isdigit(*input); ++input) {
			factor *= 0.1;
			d += factor * (*input - '0');
		}
	}

	return d;
}

static double parseExpr(const char *&input);
static double parseFactor(const char *&input) {
	if (term(input, "-")) {
		return -parseFactor(input);
	} else if (term(input, "(")) {
		double d = parseExpr(input);
		if (!term(input, ")")) assert(0);
		return d;
	} else {
		return number(input);
	}
}

static double parseMulitive(const char *&input) {
	double d = parseFactor(input);
	for (;;) {
		if (term(input, "*")) {
			d *= parseFactor(input);
		} else if (term(input, "/")) {
			d /= parseFactor(input);
		} else if (term(input, "%")) {
			d = fmod(d, parseFactor(input));
		} else {
			return d;
		}
	}
}

static double parseExpr(const char *&input) {
	double d = parseMulitive(input);
	for (;;) {
		if (term(input, "+")) {
			d += parseMulitive(input);
		} else if (term(input, "-")) {
			d -= parseMulitive(input);
		} else {
			return d;
		}
	}
}

static double eval(const char *input) {
	double d = parseExpr(input);
	whitespace(input);
	return *input == 0 ? d : 0;
}

int main() {
	assert(eval("5 - 2 * ( 1+  2) ") == -1);
	assert(eval("9- 8+ 7* 6% 5 -4 + 3") == 2);
	assert(eval("1  +2-- 3+ (-  3+ 5 * -1) ") == -2);
	assert(eval("2.0 * --0.5 + 3.125") == 4.125);
}
