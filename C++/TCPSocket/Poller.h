#ifndef POLLER_H
#define POLLER_H

#include <set>
#include <vector>

#include <poll.h>
#include <sys/epoll.h>

struct IPoller {
    enum EventFlag {
        EF_Readable = 1,
        EF_Writeable = 2,
        EF_ErrFound = 4,
    };
    struct Event {
        int flag;
        int fd;
    };

    virtual ~IPoller() {}
    virtual int size() = 0;
    virtual void add(int fd, EventFlag ef) = 0;
    virtual void update(int fd, EventFlag ef) = 0;
    virtual void del(int fd) = 0;
    virtual int wait(Event *events, int maxSize, int timeout) = 0;
};

class SelectPoller: public IPoller {
public:
    SelectPoller();
    ~SelectPoller();
    virtual int size();
    virtual void add(int fd, EventFlag ef);
    virtual void update(int fd, EventFlag ef);
    virtual void del(int fd);
    virtual int wait(Event *events, int maxSize, int timeout);
private:
    fd_set mFdSets[3];
    set<int> mFds;
};

class PollPoller: public IPoller {
public:
    PollPoller();
    ~PollPoller();
    virtual int size();
    virtual void add(int fd, EventFlag ef);
    virtual void update(int fd, EventFlag ef);
    virtual void del(int fd);
    virtual int wait(Event *events, int maxSize, int timeout);
private:
    vector<pollfd> mFds;
};

class EPollPoller: public IPoller {
public:
    EPollPoller(bool edgeTrigger);
    ~EPollPoller();
    virtual int size();
    virtual void add(int fd, EventFlag ef);
    virtual void update(int fd, EventFlag ef);
    virtual void del(int fd);
    virtual int wait(Event *events, int maxSize, int timeout);
private:
    void epControl(int op, int fd, EventFlag ef);
private:
    vector<epoll_event> mTmpEvents;
    int mEp;
    int mSize;
    bool mEdgeTrigger;
};

#endif
