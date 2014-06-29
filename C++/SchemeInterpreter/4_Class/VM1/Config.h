#ifndef CONFIG_H
#define CONFIG_H

#if defined(_WIN64) || defined(__x86_64__)
#define BIT_64
#endif

#ifdef _MSC_VER
#define alignof __alignof
#endif

// #define USE_TCMALLOC

#endif
