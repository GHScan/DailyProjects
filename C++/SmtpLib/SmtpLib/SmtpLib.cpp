// SmtpLib.cpp : 定义 DLL 应用程序的入口点。
//

#include "stdafx.h"

#include <sstream>

#include "CSmtp.h"

#ifdef _MANAGED
#pragma managed(push, off)
#endif

BOOL APIENTRY DllMain( HMODULE hModule,
                       DWORD  ul_reason_for_call,
                       LPVOID lpReserved
					 )
{
    return TRUE;
}

#ifdef _MANAGED
#pragma managed(pop)
#endif

extern "C" __declspec(dllexport)
const char*
    SendSmtpMail(
    const char *smtpServer, unsigned short smtpPort, 
    int securityType, // 0=no security;1=tls;2=ssl;3=dont set
    const char *userName, const char *password, 
    const char *senderName, const char *senderMail,
    const char *receivers,
    const char *subject, 
    int priority, // 2=high;3=normal;4=low;
    const char *content, 
    const char *attachments)
{
    static char ls_errMsg[1024] = "";

#define assertParam(exp) if (!(exp)) return strcpy_s(ls_errMsg, #exp), ls_errMsg; else;
    assertParam(smtpServer != NULL);
    assertParam(securityType == 0 || securityType == 1 || securityType == 2 || securityType == 3);
    assertParam(userName != NULL && password != NULL);
    assertParam(senderName != NULL && senderMail != NULL);
    assertParam(receivers != NULL);
    assertParam(subject != NULL);
    assertParam(priority == 2 || priority == 3 || priority == 4);
    assertParam(content != NULL);
#undef assertParam

    try
    {
        CSmtp mail;
        mail.SetSMTPServer(smtpServer, smtpPort);
        mail.SetSecurityType((SMTP_SECURITY_TYPE)securityType);

        mail.SetLogin(userName);
        mail.SetPassword(password);
        mail.SetSenderName(senderName);
        mail.SetSenderMail(senderMail);
        mail.SetReplyTo(senderMail);

        {
            std::string tokens = receivers;
            char *temp = NULL;
            char *p = strtok_s((char*)tokens.c_str(), ";", &temp);
            while (p != NULL)
            {
                mail.AddRecipient(p);
                p = strtok_s(NULL, ";", &temp);
            }
        }

        mail.SetSubject(subject);
        mail.SetXPriority((CSmptXPriority)priority);
        mail.SetXMailer("The Bat! (v3.02) Professional");

        {
            std::istringstream is(content);
            for (std::string line; getline(is, line);)
            {
                mail.AddMsgLine(line.c_str());
            }
        }

        if (attachments != NULL)
        {
            std::string tokens = attachments;
            char *temp = NULL;
            char *p = strtok_s((char*)tokens.c_str(), ";", &temp);
            while (p != NULL)
            {
                mail.AddAttachment(p);
                p = strtok_s(NULL, ";", &temp);
            }
        }

        mail.Send();
    }
    catch(ECSmtp e)
    {
        strcpy_s(ls_errMsg, e.GetErrorText().c_str());
        return ls_errMsg;
    }
    return NULL;
}