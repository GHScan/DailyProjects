#ifndef POLLER_H
#define POLLER_H

#include <set>
#include <vector>
#include <unordered_map>
#include <map>

#include <poll.h>
#include <sys/epoll.h>

struct IPoller {
    enum EventFlag {
        EF_Readable = 1,
        EF_Writeable = 2,
        EF_ErrFound = 4,
    };
    struct Event {
        void *ud;
        int flag;
    };

    virtual ~IPoller() {}
    virtual int size() = 0;
    virtual void add(int fd, void *ud, int ef) = 0;
    virtual void update(int fd, void *ud, int ef) = 0;
    virtual void del(int fd) = 0;
    virtual bool wait(vector<Event> &events, int timeout) = 0;
};

class SelectPoller: public IPoller {
public:
    SelectPoller();
    ~SelectPoller();
    virtual int size();
    virtual void add(int fd, void *ud, int ef);
    virtual void update(int fd, void *ud, int ef);
    virtual void del(int fd);
    virtual bool wait(vector<Event> &events, int timeout);
private:
    fd_set mFdSets[3];
    map<int, void*> mFd2Ud;
};

class PollPoller: public IPoller {
public:
    PollPoller();
    ~PollPoller();
    virtual int size();
    virtual void add(int fd, void *ud, int ef);
    virtual void update(int fd, void *ud, int ef);
    virtual void del(int fd);
    virtual bool wait(vector<Event> &events, int timeout);
private:
    vector<pollfd> mFds;
    unordered_map<int, void*> mFd2Ud;
};

class EPollPoller: public IPoller {
public:
    EPollPoller(bool edgeTrigger);
    ~EPollPoller();
    virtual int size();
    virtual void add(int fd, void *ud, int ef);
    virtual void update(int fd, void *ud, int ef);
    virtual void del(int fd);
    virtual bool wait(vector<Event> &events, int timeout);
private:
    void epControl(int op, int fd, void *ud, int ef);
private:
    vector<epoll_event> mTmpEvents;
    int mEp;
    int mSize;
    bool mEdgeTrigger;
};

#endif
