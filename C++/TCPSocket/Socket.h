#ifndef SOCKET_H
#define SOCKET_H

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>

#include "Utils.h"

//////////////////////////////
class HostAddress {
    ENABLE_COPY(HostAddress);
public:
    HostAddress(){}

    static HostAddress parse(const char *hostStr, short defaultPort);
    static HostAddress fromIPPort(const char *ip, short port);
    static HostAddress fromDomainPort(const char *domain, short port);

    string toString() const;
    sockaddr* getInternal() { return &mAddr; }
    const sockaddr* getInternal() const { return &mAddr; }
    socklen_t getInternalSize() const;
private:
    HostAddress(const sockaddr *addr): mAddr(*addr) {}
private:
    sockaddr mAddr;
};

class TCPSocket {
    ENABLE_COPY(TCPSocket);
public:
    TCPSocket() {}

    static TCPSocket create();
    static TCPSocket fromFd(int fd);

    void bind(const HostAddress &addr);
    void listen(int blockLog);
    void connect(const HostAddress &addr);
    bool connectAsync(const HostAddress &addr);
    TCPSocket accept(HostAddress *addr);

    void close();
    void shutdownRd();
    void shutdownWr();

    template<typename T>
    T getOption(int optName) {
        T val;
        getOption(optName, (char*)&val, sizeof(val));
        return val;
    }
    template<typename T>
    void setOption(int optName, const T& val) {
        setOption(optName, (char*)&val, sizeof(val));
    }
    void setNonBlocking(bool b);
    void setReuseAddress();

    int getFd() const { return mFd; }

private:
    TCPSocket(int fd): mFd(fd){}
private:
    void getOption(int optName, char *buf, int buflen);
    void setOption(int optName, const char *buf, int buflen);
private:
    int mFd;
};

#endif
