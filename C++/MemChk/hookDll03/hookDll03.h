#pragma once

#ifdef HOOKDLL03_EXPORTS
#define HOOKDLL_API __declspec(dllexport)
#else
#define HOOKDLL_API __declspec(dllimport)
#endif

#define ENABLE_MEMORY_CHECK 1

HOOKDLL_API void dumpMemoryLeaksToFile(FILE *f, size_t maxDumpCnt = -1);
HOOKDLL_API void dumpMemoryLeaksToDebug(size_t maxDumpCnt = -1);