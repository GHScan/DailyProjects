// hookDll03.cpp : 定义 DLL 应用程序的入口点。
//

#include "stdafx.h"

#include "APIHook.h"
#include "DllComponent.h"

bool g_dllWorking = false;

BOOL APIENTRY DllMain( HANDLE hModule, 
                       DWORD  ul_reason_for_call, 
                       LPVOID lpReserved
					 )
{
    switch (ul_reason_for_call)
    {
    case DLL_PROCESS_ATTACH:
        {
            IDllComponent *header = IDllComponent::getHeader();
            while (header != NULL)
            {
                if (header->isEnable() && !header->setup())
                {
                    break;
                }
                else
                {
                    header = header->getNext();
                }
            }
            if (header != NULL)
            {
                IDllComponent *header2 = IDllComponent::getHeader();
                while (header2 != header)
                {
                    header2->cleanup();
                    header2 = header2->getNext();
                }

                g_dllWorking = false;
            }
            else
            {
                g_dllWorking = true;
            }
        }

        break;
    case DLL_THREAD_ATTACH :
        break;
    case DLL_THREAD_DETACH:
        break;
    case DLL_PROCESS_DETACH:
        if (g_dllWorking)
        {
            g_dllWorking = false;

            IDllComponent *header = IDllComponent::getHeader();
            while (header != NULL)
            {
                header->cleanup();
                header = header->getNext();
            }
        }
        break;
    default:
        break;
    }

    return TRUE;
}

static IDllComponent*& getHeaderSafe()
{
    static IDllComponent* ls_header = NULL;
    return ls_header;
}

IDllComponent* IDllComponent::getHeader()
{
    return getHeaderSafe();
}

IDllComponent::IDllComponent()
{
    if (getHeaderSafe() == NULL)
    {
        getHeaderSafe() = this;
    }
    else
    {
        m_next = getHeaderSafe();
        getHeaderSafe() = this;
    }
}

IDllComponent::~IDllComponent()
{
    if (getHeaderSafe() == this)
    {
        getHeaderSafe() = m_next;
    }
    else
    {
        IDllComponent *header = getHeaderSafe();
        while (header->getNext() != this)
        {
            header = header->getNext();
        }
        header->m_next = m_next;
    }
}