// SmtpTest.cpp : �������̨Ӧ�ó������ڵ㡣
//

#include "stdafx.h"

#include <windows.h>

bool SendMail(
    const char *receivers, 
    const char *subject,
    const char *content, 
    const char* attachments)
{
    HMODULE dll = GetModuleHandle("SmtpLib.dll");
    if (dll == NULL) dll = LoadLibrary("SmtpLib.dll");
    if (dll == NULL) return false;

    typedef 
    const char*
        (*T_SendSmtpMail)(
        const char *smtpServer, unsigned short smtpPort, 
        int securityType, // 0=no security;1=tls;2=ssl;3=dont set
        const char *userName, const char *password, 
        const char *senderName, const char *senderMail,
        const char *receivers,
        const char *subject, 
        int priority, // 2=high;3=normal;4=low;
        const char *content, 
        const char *attachments);
    T_SendSmtpMail f = (T_SendSmtpMail)GetProcAddress(dll, "SendSmtpMail");
    if (f == NULL) return false;

    const char *errMsg = f(
        "smtp.qq.com", 465,
        2,
        "376266015", "3762scan66015",
        "Scan[�Զ�����]", "376266015@qq.com",
        receivers,
        subject,
        3,
        content,
        attachments);

    if (errMsg == NULL) return true;
    OutputDebugString("SendMail failed : ");
    OutputDebugString(errMsg);
    return false;
}

int main()
{
    SendMail("ppscan@qq.com;376266015@qq.com", "�Զ����Ͳ���", "��һ��\n�ڶ���\n������\noyes��", "D:/Downloads/md5.h");

	return 0;
}