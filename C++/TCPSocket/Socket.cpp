#include "pch.h"

#include "Socket.h"

#include <unistd.h>
#include <fcntl.h>

HostAddress HostAddress::parse(const char *hostStr, short defaultPort) {
    short port = defaultPort;

    string ip;
    if (const char *p = strchr(hostStr, ':')) {
        ip = string(hostStr, p);
        port = (short)atoi(p + 1);
    } else {
        ip = hostStr;
    }

    in_addr ia;
    if (::inet_aton(ip.c_str(), &ia) != 0) {
        return fromIPPort(ip.c_str(), port);
    } else {
        return fromDomainPort(ip.c_str(), port);
    }
}
HostAddress HostAddress::fromIPPort(const char *ip, short port) {
    in_addr ia;
    P_ENSURE(::inet_aton(ip, &ia) != 0);

    sockaddr_in a;
    a.sin_family = AF_INET;
    a.sin_port = htons(port);
    a.sin_addr = ia;
    return HostAddress((sockaddr*)&a);
}
HostAddress HostAddress::fromDomainPort(const char *domain, short port) {
    char buf[512];
    int err;
    hostent ret, *result;
    ::gethostbyname_r(domain, &ret, buf, sizeof(buf), &result, &err);
    P_ENSURE_ERR(result != nullptr, err);

    sockaddr_in a;
    a.sin_family = AF_INET;
    a.sin_port = htons(port);
    a.sin_addr = *((in_addr**)ret.h_addr_list)[0];
    return HostAddress((sockaddr*)&a);
}
string HostAddress::toString() const {
    sockaddr_in *pa = (sockaddr_in*)&mAddr;

    char buf[32];
    ::inet_ntop(AF_INET, &pa->sin_addr, buf, sizeof(buf));
    return format("%s:%d", buf, ntohs(pa->sin_port));
}
socklen_t HostAddress::getInternalSize() const {
    return sizeof(sockaddr_in);
}

TCPSocket TCPSocket::create() {
    int fd = ::socket(AF_INET, SOCK_STREAM, 0);
    P_ENSURE(fd != -1);
    return TCPSocket(fd);
}
TCPSocket TCPSocket::fromFd(int fd) {
    return TCPSocket(fd);
}
void TCPSocket::bind(const HostAddress &addr) {
    P_ENSURE(::bind(mFd, addr.getInternal(), addr.getInternalSize()) != -1);
}
void TCPSocket::listen(int blockLog) {
    P_ENSURE(::listen(mFd, blockLog) != -1);
}
void TCPSocket::connect(const HostAddress &addr) {
    P_ENSURE(::connect(mFd, addr.getInternal(), addr.getInternalSize()) != -1);
}
bool TCPSocket::connectAsync(const HostAddress &addr) {
    int err = ::connect(mFd, addr.getInternal(), addr.getInternalSize());
    if (err == 0) return true;
    P_ENSURE(errno == EINPROGRESS);
    return false;
}
TCPSocket TCPSocket::accept(HostAddress *addr) {
    socklen_t size = addr->getInternalSize();
    int fd = ::accept(mFd, addr->getInternal(), &size);
    P_ENSURE(fd != -1 && size == addr->getInternalSize())(size);
    return fromFd(fd);
}

void TCPSocket::close() {
    CLOSE(mFd);
}
void TCPSocket::shutdownRd() {
    P_ENSURE(::shutdown(mFd, SHUT_RD) != -1);
}
void TCPSocket::shutdownWr() {
    P_ENSURE(::shutdown(mFd, SHUT_WR) != -1);
}

void TCPSocket::getOption(int optName, char *buf, int buflen) {
    socklen_t len = buflen;
    P_ENSURE(::getsockopt(mFd, SOL_SOCKET, optName, buf, &len) != -1);
    P_ENSURE((int)len == buflen)(len)(buflen);
}
void TCPSocket::setOption(int optName, const char *buf, int buflen) {
    P_ENSURE(::setsockopt(mFd, SOL_SOCKET, optName, buf, buflen) != -1)(buflen);
}
void TCPSocket::setNonBlocking(bool b) {
    int flag = ::fcntl(mFd, F_GETFL);
    P_ENSURE(flag != -1);

    if (b) flag |= O_NONBLOCK;
    else flag &= ~O_NONBLOCK;

    P_ENSURE(::fcntl(mFd, F_SETFL, flag) != -1);
}
void TCPSocket::setReuseAddress() {
    setOption(SO_REUSEADDR, 1);
}
