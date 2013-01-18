#include "StdAfx.h"

#include <cassert>

#include <vector>
#include <string>
#include <exception>

#include <process.h>

#include "IPlayer.h"
#include "IGobangAlgo.h"

class ComputerPlayer:
    public IPlayer
{
public:
    ComputerPlayer(const char*name, IGobangAlgo *algo);
    ~ComputerPlayer();

protected:
    virtual void init(int size, HWND wnd);
    virtual void destroy();

    virtual void reset();
    virtual void notifyPlaced(int x, int y);
    virtual bool requirePlace(int *x, int *y);

    virtual bool canPeerWin() const;
    virtual bool canWin() const;

    virtual const char* getName();

    virtual void* getAlgo();

private:
    static unsigned __stdcall _threadFunc(void *);
    void threadFunc();

private:    
    HWND    m_wnd;

    HANDLE  m_td;
    HANDLE  m_evtExit;
    HANDLE  m_evtThink;

    int     m_x, m_y;

    IGobangAlgo  *m_algo;

    std::string  m_name;

    bool         m_hasEnd;
};

ComputerPlayer::ComputerPlayer(const char*name, IGobangAlgo *algo):
m_algo(algo), m_wnd(NULL), m_td(NULL), m_evtThink(NULL), m_evtExit(NULL),
m_x(0), m_y(0), m_name(name)
{

}

ComputerPlayer::~ComputerPlayer()
{
    delete m_algo;
    m_algo = NULL;
}

unsigned __stdcall ComputerPlayer::_threadFunc(void *_this)
{
    static_cast<ComputerPlayer*>(_this)->threadFunc();

    return 0;
}

void ComputerPlayer::threadFunc()
{
    try
    {
        for (;;)
        {
            HANDLE events[] = {m_evtThink, m_evtExit};

            DWORD obj = WaitForMultipleObjects(2, events, FALSE, -1);

            if (obj == WAIT_OBJECT_0 + 1)
            {
                break;
            }

            m_hasEnd = m_algo->thisGo(m_x, m_y);

            PostMessage(m_wnd, UM_PCPLAYER_THINKOVER, 0, 0);
        }
    }
    catch(std::exception& e)
    {
        MessageBox(NULL, e.what(), "Òì³£", MB_OK);
    }
}

void ComputerPlayer::init(int size, HWND wnd)
{
    m_wnd = wnd;

    m_evtThink = CreateEvent(NULL, FALSE, FALSE, NULL);

    m_evtExit = CreateEvent(NULL, FALSE, FALSE, NULL);
    _beginthreadex(NULL, 0, _threadFunc, this, 0, (unsigned*)&m_td);

    m_algo->init(size);
}

void ComputerPlayer::destroy()
{
    if (m_td != NULL)
    {
        SetEvent(m_evtExit);
        DWORD dw = WaitForSingleObject(m_td, 3000);
        if (dw != WAIT_FAILED && dw != WAIT_OBJECT_0)
        {
            CloseHandle(m_td);
        }
        m_td = NULL;
    }

    CloseHandle(m_evtExit);
    m_evtExit = NULL;

    CloseHandle(m_evtThink);
    m_evtThink = NULL;

    delete this;
}

void ComputerPlayer::notifyPlaced(int x, int y)
{
    m_algo->peerGo(x, y);
}

bool ComputerPlayer::requirePlace(int *x, int *y)
{
    if (x == NULL)
    {
        SetEvent(m_evtThink);
        return false;
    }
    else
    {
        *x = m_x;
        *y = m_y;
        return m_hasEnd;
    }
}

void ComputerPlayer::reset()
{
    m_algo->reset();
}

const char* ComputerPlayer::getName()
{
    return m_name.c_str();
}

bool ComputerPlayer::canPeerWin() const
{
    return m_algo->canPeerWin();
}

bool ComputerPlayer::canWin() const
{
    return m_algo->canWin();
}

void* ComputerPlayer::getAlgo()
{
    return m_algo;
}

std::map<std::string, algoCreator>& getAlgoMap()
{
    static std::map<std::string, algoCreator> ls_map;
    return ls_map;
}

DLL_ABI IPlayer* IPlayer_create(const char* type, const char* name)
{
    std::map<std::string, algoCreator>::iterator iter = getAlgoMap().find(type);
    if (iter != getAlgoMap().end())
    {
        return new ComputerPlayer(name, iter->second());
    }

    return NULL;
}

DLL_ABI void IPlayer_getAIList(std::vector<std::string> &ais)
{
    std::map<std::string, algoCreator>::iterator iter = getAlgoMap().begin();
    while (iter != getAlgoMap().end())
    {
        ais.push_back(iter->first);
        ++iter;
    }
}