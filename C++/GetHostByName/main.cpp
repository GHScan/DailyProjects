#include "pch.h"

#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>

int main(int argc, char *argv[]) {
    if (argc < 2) return 0;

    in_addr addr;
    hostent *host;
    if (inet_aton(argv[1], &addr) != 0) {
        host = gethostbyaddr(&addr, sizeof(addr), AF_INET);
    } else {
        host = gethostbyname(argv[1]);
    }

    if (host == NULL) herror("gethost failed");
    else {
        printf("offical name : %s\n", host->h_name);
        for (char ** alias = host->h_aliases; *alias; ++alias) {
            printf("\talias:%s\n", *alias);
        }
        for (in_addr_t **p = (in_addr_t**)host->h_addr_list; *p; ++p) {
            addr.s_addr = **p;
            printf("\taddr:%s\n", inet_ntoa(addr));
        }
    }
}
