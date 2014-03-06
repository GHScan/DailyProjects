#ifndef POLLER_H
#define POLLER_H

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
    static IPoller* create(const char *type);
};


#endif
