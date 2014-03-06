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
        int fd;
        int flag;
    };

    virtual ~IPoller() {}
    virtual int size() = 0;
    virtual void add(int fd, int ef) = 0;
    virtual void update(int fd, int ef) = 0;
    virtual void del(int fd) = 0;
    virtual bool wait(vector<Event> &events, int timeout) = 0;
};

class SelectPoller: public IPoller {
public:
    SelectPoller();
    ~SelectPoller();
    virtual int size();
    virtual void add(int fd, int ef);
    virtual void update(int fd, int ef);
    virtual void del(int fd);
    virtual bool wait(vector<Event> &events, int timeout);
private:
    fd_set mFdSets[3];
    set<int> mFds;
};

class PollPoller: public IPoller {
public:
    PollPoller();
    ~PollPoller();
    virtual int size();
    virtual void add(int fd, int ef);
    virtual void update(int fd, int ef);
    virtual void del(int fd);
    virtual bool wait(vector<Event> &events, int timeout);
private:
    vector<pollfd> mFds;
};

class EPollPoller: public IPoller {
public:
    EPollPoller(bool edgeTrigger);
    ~EPollPoller();
    virtual int size();
    virtual void add(int fd, int ef);
    virtual void update(int fd, int ef);
    virtual void del(int fd);
    virtual bool wait(vector<Event> &events, int timeout);
private:
    void epControl(int op, int fd, int ef);
private:
    vector<epoll_event> mTmpEvents;
    int mEp;
    int mSize;
    bool mEdgeTrigger;
};

#endif
