// Test13.cpp : 定义控制台应用程序的入口点。
//

#include "stdafx.h"


#include <Winsock2.h>
#pragma comment(lib, "Ws2_32.lib")

int main() {
	const char *str = "localhost";

	WORD wVersionRequested;
	WSADATA wsaData;
	int err;
	wVersionRequested = MAKEWORD(2, 2);
	err = WSAStartup(wVersionRequested, &wsaData);
	if (err != 0) { return 1; }
	if (LOBYTE(wsaData.wVersion) != 2 ||
		HIBYTE(wsaData.wVersion) != 2) {
		WSACleanup();
		return 1;
	}


	in_addr addr;
	hostent *host;
	if ((addr.S_un.S_addr = inet_addr(str)) != INADDR_NONE) {
		host = gethostbyaddr((char*)&addr, sizeof(addr), AF_INET);
	}
	else {
		host = gethostbyname(str);
	}

	if (host == NULL) puts("gethost failed");
	else {
		printf("offical name : %s\n", host->h_name);
		for (char ** alias = host->h_aliases; *alias; ++alias) {
			printf("\talias:%s\n", *alias);
		}
		for (ULONG **p = (ULONG**)host->h_addr_list; *p; ++p) {
			addr.S_un.S_addr = **p;
			printf("\taddr:%s\n", inet_ntoa(addr));
		}
	}

}