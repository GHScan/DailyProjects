// Test05.cpp : 定义控制台应用程序的入口点。
//

#include "stdafx.h"

#include <cassert>

#include <map>
#include <vector>
#include <string>

#include <boost/function.hpp>
#include <boost/bind.hpp>

#define _WIN32_WINNT 0x0501
#define WINVER 0x0501
#include <windows.h>
#include <process.h>

#define TO_STR2(s) #s
#define TO_STR(s) TO_STR2(s)

#ifdef _DEBUG
#define myAssert(b) assert(b)
#else
#define myAssert(b) if (b); else throw TO_STR(b)
#endif

template<typename T1, typename T2>
inline T1 boundup(T1 a, T2 align)
{
    return (a + align - 1) / align * align;
}

volatile long g_threadCnt = 0;
void incThreadCnt()
{
    InterlockedIncrement(&g_threadCnt);
}
void decThreadCnt()
{
    InterlockedDecrement(&g_threadCnt);
}
long getThreadCnt()
{
    return g_threadCnt;
}
class EnsureAllThreadExit
{
public:
    ~EnsureAllThreadExit()
    {
        myAssert(getThreadCnt() == 0);
    }
}g_ensureAllThreadExit;

class IOCP
{
public:
    typedef boost::function<void(DWORD numBytesTransed, LPOVERLAPPED overlapped)> Callback;
public:
    IOCP();
    ~IOCP();

    void addDevice(HANDLE device, Callback callback);
    void removeDevice(HANDLE device);
    bool post(HANDLE device, DWORD numOfBytesTransed, LPOVERLAPPED overlap);

private:
    typedef std::map<HANDLE, Callback>   Device2CallbackMap;
    typedef std::vector<HANDLE>          ThreadHandleVec;

private:
    static const ULONG_PTR COMPKEY_ENDTHREAD = 0;

private:
    static unsigned __stdcall threadProc(void* _this);

private:
    HANDLE m_iocpHandle;
    ThreadHandleVec m_threadHandles;
    Device2CallbackMap  m_device2Callback;
    volatile bool m_endThread;
    CRITICAL_SECTION m_cs;
};

IOCP::IOCP()
{
    InitializeCriticalSectionAndSpinCount(&m_cs, 4000);

    SYSTEM_INFO sysInfo = {0};
    GetSystemInfo(&sysInfo);
    m_iocpHandle = CreateIoCompletionPort(INVALID_HANDLE_VALUE, NULL, 0, sysInfo.dwNumberOfProcessors);
    myAssert(m_iocpHandle != NULL);

    m_threadHandles.resize(sysInfo.dwNumberOfProcessors * 2);
    for (size_t i = 0; i < m_threadHandles.size(); ++i)
    {
        m_threadHandles[i] = (HANDLE)_beginthreadex(NULL, 0, &IOCP::threadProc, this, 0, NULL);
    }
}

IOCP::~IOCP()
{
    while (!m_threadHandles.empty())
    {
        post((HANDLE)COMPKEY_ENDTHREAD, 0, NULL);
        DWORD ret = WaitForMultipleObjects((DWORD)m_threadHandles.size(), &m_threadHandles[0], FALSE, INFINITE);
        if (ret >= WAIT_OBJECT_0)
        {
            int i = ret - WAIT_OBJECT_0;
            CloseHandle(m_threadHandles[i]);
            m_threadHandles.erase(m_threadHandles.begin() + i);
        }
        else myAssert(0);
    }

    if (m_iocpHandle != NULL) 
    {
        CloseHandle(m_iocpHandle); m_iocpHandle = NULL;
    }

    DeleteCriticalSection(&m_cs);
}

bool IOCP::post(HANDLE device, DWORD numOfBytesTransed, LPOVERLAPPED overlap)
{
    return PostQueuedCompletionStatus(m_iocpHandle, numOfBytesTransed, (ULONG_PTR)device, overlap) == TRUE;
}

void IOCP::addDevice(HANDLE device, Callback callback)
{
    myAssert(m_device2Callback.count(device) == 0);
    HANDLE r = CreateIoCompletionPort(device, m_iocpHandle, (ULONG_PTR)device, 0);
    myAssert(r == m_iocpHandle);
    m_device2Callback[device] = callback;
}

void IOCP::removeDevice(HANDLE device)
{
    myAssert(m_device2Callback.count(device) > 0);
    m_device2Callback.erase(device);
}

unsigned __stdcall IOCP::threadProc(void* arg)
{
    incThreadCnt();

    IOCP *_this = (IOCP*)arg;

    DWORD numOfBytesTransed = 0;
    ULONG_PTR compKey = 0;
    LPOVERLAPPED overlap = NULL;
    for (;;)
    {
        if (GetQueuedCompletionStatus(_this->m_iocpHandle, &numOfBytesTransed, &compKey, &overlap, INFINITE))
        {
            if (compKey == COMPKEY_ENDTHREAD) break; // exit
            
            Callback callback;
            EnterCriticalSection(&_this->m_cs);
            callback = _this->m_device2Callback[(HANDLE)compKey];
            LeaveCriticalSection(&_this->m_cs);

            myAssert(callback != NULL);
            callback(numOfBytesTransed, overlap);
        }
        else
        {
            myAssert(0);

            DWORD lastError = GetLastError();
            if (overlap == NULL)
            {
                if (lastError == WAIT_TIMEOUT) myAssert(0);
                else
                {
                }
            }
            else
            {
                overlap; compKey; numOfBytesTransed;
            }
        }
    }

    decThreadCnt();

    return 0;
}

class FileData : 
    public OVERLAPPED
{
public:
    FileData();
    ~FileData();
    void* getBuf();
    static size_t getSize() { return 1024 * 32; }

private:
    std::vector<char> m_buf;
};

FileData::FileData():
m_buf(getSize(), 0)
{
}

FileData::~FileData()
{
}

void* FileData::getBuf()
{
    if (m_buf.empty()) return NULL;
    return &m_buf[0];
}

class File
{
public:
    File(const char *fileName, bool read, IOCP *iocp, IOCP::Callback callback);
    ~File();

    HANDLE getHandle() { return m_fileHandle; }

    bool read(size_t offset, FileData& data);
    bool write(size_t offset, FileData& data);

private:
    HANDLE m_fileHandle;
    IOCP *m_iocp;
};

File::File(const char *fileName, bool read, IOCP *iocp, IOCP::Callback callback):
m_fileHandle(INVALID_HANDLE_VALUE)
{
    m_fileHandle = CreateFile(
        fileName, 
        read ? GENERIC_READ : GENERIC_WRITE, 
        0, NULL, 
        read ? OPEN_EXISTING : CREATE_ALWAYS, 
        FILE_FLAG_OVERLAPPED | FILE_FLAG_NO_BUFFERING | FILE_ATTRIBUTE_NORMAL,
        NULL);
    myAssert(m_fileHandle != INVALID_HANDLE_VALUE);

    m_iocp = iocp;
    m_iocp->addDevice(m_fileHandle, callback);
}

File::~File()
{
    m_iocp->removeDevice(m_fileHandle);
    m_iocp = NULL;

    if (m_fileHandle != INVALID_HANDLE_VALUE) 
    {
        CloseHandle(m_fileHandle);
        m_fileHandle = INVALID_HANDLE_VALUE;
    }
}

bool File::read(size_t offset, FileData& data)
{
    myAssert(offset % 1024 == 0);
    memset(&data, 0, sizeof(OVERLAPPED));
    data.Offset = offset;
    return ReadFile(m_fileHandle, data.getBuf(), data.getSize(), NULL, &data) == TRUE || 
        GetLastError() == ERROR_IO_PENDING;
}

bool File::write(size_t offset, FileData& data)
{
    myAssert(offset % 1024 == 0);
    memset(&data, 0, sizeof(OVERLAPPED));
    data.Offset = offset;
    return WriteFile(m_fileHandle, data.getBuf(), data.getSize(), NULL, &data) == TRUE || 
        GetLastError() == ERROR_IO_PENDING;
}

struct Context
{
    File *srcFile, *destFile;
    volatile long remainSrcFileDataCnt;
    volatile long pendingWriteReq;
    volatile long pendingReadReq;
    volatile long readOffset;

    void wait()
    {
        while (remainSrcFileDataCnt > 0 || pendingWriteReq > 0 || pendingReadReq > 0) Sleep(100);
    }
};
void onReaded(Context *context, DWORD numBytesTransed, LPOVERLAPPED overlapped)
{
    InterlockedDecrement(&context->pendingReadReq);
    InterlockedIncrement(&context->pendingWriteReq);
    myAssert(context->destFile->write(overlapped->Offset, (FileData&)*overlapped));
}

void onWrited(Context *context, DWORD numBytesTransed, LPOVERLAPPED overlapped)
{
    InterlockedDecrement(&context->pendingWriteReq);
    
    if (context->remainSrcFileDataCnt == 0) return;
    InterlockedDecrement(&context->remainSrcFileDataCnt);
    FileData& fd((FileData&)*overlapped);
    context->srcFile->read(InterlockedExchangeAdd(&context->readOffset, fd.getSize()), fd);
}

int main()
{
    incThreadCnt();

    DWORD startTick = GetTickCount();

    const char *fileNameSrc = "D:/Downloads/1";
    const char *fileNameDest = "D:/Downloads/2";
    DWORD srcFileSize;

    {
        IOCP iocp;
        Context context;
        File srcFile(fileNameSrc, true, &iocp, boost::bind(&onReaded, &context, _1, _2));
        File destFile(fileNameDest, false, &iocp, boost::bind(&onWrited, &context, _1, _2));

        srcFileSize = GetFileSize(srcFile.getHandle(), NULL);

        FileData fds[8];

        context.srcFile = &srcFile; context.destFile = &destFile;
        context.pendingWriteReq = 0; context.pendingReadReq = 0;
        context.remainSrcFileDataCnt = boundup(srcFileSize, fds[0].getSize()) / fds[0].getSize();
        context.readOffset = 0;

        int startLoop = min(_countof(fds), context.remainSrcFileDataCnt);
        for (int i = 0; i < startLoop; ++i)
        {
            InterlockedDecrement(&context.remainSrcFileDataCnt);
            InterlockedIncrement(&context.pendingReadReq);
            myAssert(srcFile.read(
                InterlockedExchangeAdd(&context.readOffset, fds[0].getSize()), 
                fds[i]));
        }

        context.wait();
    }

    {
        HANDLE h = CreateFile(fileNameDest, GENERIC_WRITE, 0, NULL, OPEN_EXISTING, 0, NULL);
        myAssert(h != INVALID_HANDLE_VALUE);
        SetFilePointer(h, srcFileSize, NULL, FILE_BEGIN);
        SetEndOfFile(h);
        CloseHandle(h);
    }

    /*{
        FILE *srcf = fopen(fileNameSrc, "rb");
        FILE *destF = fopen(fileNameDest, "wb");
        char buf[1 << 18];
        for (;;)
        {
            size_t sz = fread(buf, 1, _countof(buf), srcf);
            srcFileSize += sz;
            if (sz == 0) break;
            fwrite(buf, 1, sz, destF);
        }
        fclose(srcf);
        fclose(destF);
    }*/

    cout << (float)srcFileSize / (GetTickCount() - startTick) << endl;
    cin.get();

    decThreadCnt();
}