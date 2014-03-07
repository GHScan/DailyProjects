#define APPTYPE_BlockingClient      1
#define APPTYPE_NonblockingClient   2
#define APPTYPE_BlockingServer      3

#ifndef APPTYPE
#define APPTYPE APPTYPE_BlockingClient
#endif

#if APPTYPE == APPTYPE_BlockingClient
#include "BlockingClient/main.cpp"
#elif APPTYPE == APPTYPE_NonblockingClient
#include "NonblockingClient/main.cpp"
#elif APPTYPE == APPTYPE_BlockingServer
#include "BlockingServer/main.cpp"
#endif
