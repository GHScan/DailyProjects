
#ifndef COMMON_H
#define COMMON_H

#define ASSERT(b, msg) if (b); else (cout << "assert failed: " << msg), assert(b)

string format(const char *fmt, ...);

#endif
