#ifndef ASSERT_H
#define ASSERT_H

#include "Config.h"

int by_abort(const char *e, const char *message, const char *file, int line);

#define by_RASSERT(e, message)       ((void)((e) || by_abort(#e, message, __FILE__, __LINE__)))

#ifdef by_DEBUG
#define by_ASSERT(e, message)        ((void)((e) || by_abort(#e, message, __FILE__, __LINE__)))
#else
#define by_ASSERT(e, message)        ((void)(e))
#endif

#endif
