#define APPTYPE_BlockingClient      
//#define APPTYPE_NonblockingClient   
//#define APPTYPE_BlockingServer      
//#define APPTYPE_ProactorServer      
//#define APPTYPE_ProactorServer2     
//#define APPTYPE_ProactorClient      
//#define APPTYPE_WebProxy            

#if defined(APPTYPE_BlockingClient)
#include "BlockingClient/main.cpp"
#elif defined(APPTYPE_NonblockingClient)
#include "NonblockingClient/main.cpp"
#elif defined(APPTYPE_BlockingServer)
#include "BlockingServer/main.cpp"
#elif defined(APPTYPE_ProactorServer)
#include "ProactorServer/main.cpp"
#elif defined(APPTYPE_ProactorServer2)
#include "ProactorServer2/main.cpp"
#elif defined(APPTYPE_ProactorClient)
#include "ProactorClient/main.cpp"
#elif defined(APPTYPE_WebProxy)
#include "WebProxy/main.cpp"
#endif
