#pragma once

void crtAllocation_beginHook(size_t minHookBytes);
void crtAllocation_endHook();
void crtAllocation_dumpResults(size_t minDumpBytes, const char *file = NULL);
void crtAllocation_clearResults();