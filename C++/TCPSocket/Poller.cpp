#include "pch.h"

#include <sys/select.h>
#include <unistd.h>

#include "Utils.h"
#include "Poller.h"

SelectPoller::SelectPoller() {
    FD_ZERO(mFdSets + 0);
    FD_ZERO(mFdSets + 1);
    FD_ZERO(mFdSets + 2);
}
SelectPoller::~SelectPoller() {
    ENSURE(mFds.empty());
}
void SelectPoller::add(int fd, EventFlag ef) {
    ENSURE(fd < FD_SETSIZE)(fd);

    if (ef & EF_Readable) FD_SET(fd, mFdSets + 0);
    if (ef & EF_Writeable) FD_SET(fd, mFdSets + 1);
    FD_SET(fd, mFdSets + 2);
    mFds.insert(fd);
}
void SelectPoller::update(int fd, EventFlag ef) {
    del(fd);
    add(fd, ef);
}
void SelectPoller::del(int fd) {
    ENSURE(fd < FD_SETSIZE)(fd);

    FD_CLR(fd, mFdSets + 0);
    FD_CLR(fd, mFdSets + 1);
    FD_CLR(fd, mFdSets + 2);
    mFds.erase(fd);
}
int SelectPoller::wait(Event *events, int maxSize, int timeout) {
    ENSURE(size() > 0);

    fd_set tmpFdSets[3] = { mFdSets[0], mFdSets[1], mFdSets[2]};
    timeval tval = { timeout / 1000, (timeout % 1000) * 1000 };

    int n = ::select(*mFds.rbegin() + 1, mFdSets + 0, mFdSets + 1, mFdSets + 2, &tval);
    P_ENSURE(n >= 0);
    if (n == 0) return 0;

    int eidx = 0;
    for (int fd : mFds) {
        Event *event = events + eidx;
        event->flag = 0;
        if (FD_ISSET(fd, tmpFdSets + 0)) event->flag |= EF_Readable;
        if (FD_ISSET(fd, tmpFdSets + 1)) event->flag |= EF_Writeable;
        if (FD_ISSET(fd, tmpFdSets + 2)) event->flag |= EF_ErrFound;
        if (event->flag != 0) {
            event->fd = fd;
            if (++eidx >= maxSize) break;
        }
    }
    return eidx;
}
int SelectPoller::size() {
    return mFds.size();
}

//////////////////////////////
PollPoller::PollPoller() {
}
PollPoller::~PollPoller() {
    ENSURE(mFds.empty());
}
int PollPoller::size() {
    return mFds.size();
}
void PollPoller::add(int fd, EventFlag ef) {
    pollfd pf = {fd};
    if (ef & EF_Readable) pf.events |= POLLIN;
    if (ef & EF_Writeable) pf.events |= POLLOUT;
    mFds.push_back(pf);
}
void PollPoller::update(int fd, EventFlag ef) {
    del(fd);
    add(fd, ef);
}
void PollPoller::del(int fd) {
    mFds.erase(find_if(mFds.begin(), mFds.end(), [fd](const pollfd& pf){ return pf.fd == fd; }));
}
int PollPoller::wait(Event *events, int maxSize, int timeout) {
    ENSURE(size() > 0);

    for (pollfd &pf : mFds) pf.revents = 0;

    int n = ::poll(&mFds[0], mFds.size(), timeout);
    P_ENSURE(n >= 0);
    if (n == 0) return 0;

    int eidx = 0;
    for (pollfd &pf : mFds) {
        Event *event = events + eidx;
        event->flag = 0;
        if (pf.revents & POLLIN) event->flag |= EF_Readable;
        if (pf.revents & POLLOUT) event->flag |= EF_Writeable;
        if (pf.revents & (POLLERR | POLLHUP | POLLNVAL | POLLRDHUP))  event->flag |= EF_ErrFound;
        if (event->flag != 0) {
            event->fd = pf.fd;
            if (++eidx >= maxSize) break;
        }
    }

    ENSURE(eidx == n);
    return eidx;
}

//////////////////////////////
EPollPoller::EPollPoller(bool edgeTrigger): mSize(0), mEdgeTrigger(edgeTrigger) {
    mEp = ::epoll_create(1);
    P_ENSURE(mEp != -1);
}
EPollPoller::~EPollPoller() {
    ENSURE(mSize == 0);
    ::close(mEp);
}
int EPollPoller::size() {
    return mSize;
}
void EPollPoller::add(int fd, EventFlag ef) {
    ++mSize;
    epControl(EPOLL_CTL_ADD, fd, ef);
}
void EPollPoller::update(int fd, EventFlag ef) {
    epControl(EPOLL_CTL_MOD, fd, ef);
}
void EPollPoller::del(int fd) {
    --mSize;
    epControl(EPOLL_CTL_DEL, fd, (EventFlag)0);
}
void EPollPoller::epControl(int op, int fd, EventFlag ef) {
    epoll_event event = {0};
    event.data.fd = fd;
    if (ef & EF_Readable) event.events |= EPOLLIN;
    if (ef & EF_Writeable) event.events |= EPOLLOUT;
    if (mEdgeTrigger) event.events |= EPOLLET;
    P_ENSURE(::epoll_ctl(mEp, op, fd, &event) == 0);
}
int EPollPoller::wait(Event *events, int maxSize, int timeout) {
    ENSURE(size() > 0);

    mTmpEvents.resize(maxSize);

    int n = ::epoll_wait(mEp, &mTmpEvents[0], mTmpEvents.size(), timeout);
    P_ENSURE(n >= 0);
    if (n == 0) return 0;

    for (int i = 0; i < n; ++i) {
        epoll_event *event = &mTmpEvents[i];
        if (event->events & EPOLLIN) events[i].flag |= EF_Readable;
        if (event->events & EPOLLOUT) events[i].flag |= EF_Writeable;
        if (event->events & (EPOLLRDHUP | EPOLLERR | EPOLLHUP)) events[i].flag |= EF_ErrFound;
    }

    return n;
}
