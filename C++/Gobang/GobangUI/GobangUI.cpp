// GobangUI.cpp : ����Ӧ�ó��������Ϊ��
//

#include "stdafx.h"

#include <exception>

#include "GobangUI.h"
#include "DlgGobangUI.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#endif


// CAppGobangUI

BEGIN_MESSAGE_MAP(CAppGobangUI, CWinApp)
	ON_COMMAND(ID_HELP, &CWinApp::OnHelp)
END_MESSAGE_MAP()


// CAppGobangUI ����

CAppGobangUI::CAppGobangUI()
{
	// TODO: �ڴ˴���ӹ�����룬
	// ��������Ҫ�ĳ�ʼ�������� InitInstance ��

    // _CrtSetBreakAlloc(4039);
}


// Ψһ��һ�� CAppGobangUI ����

CAppGobangUI theApp;


// CAppGobangUI ��ʼ��

BOOL CAppGobangUI::InitInstance()
{
	// ���һ�������� Windows XP �ϵ�Ӧ�ó����嵥ָ��Ҫ
	// ʹ�� ComCtl32.dll �汾 6 ����߰汾�����ÿ��ӻ���ʽ��
	//����Ҫ InitCommonControlsEx()�����򣬽��޷��������ڡ�
	INITCOMMONCONTROLSEX InitCtrls;
	InitCtrls.dwSize = sizeof(InitCtrls);
	// ��������Ϊ��������Ҫ��Ӧ�ó�����ʹ�õ�
	// �����ؼ��ࡣ
	InitCtrls.dwICC = ICC_WIN95_CLASSES;
	InitCommonControlsEx(&InitCtrls);

	CWinApp::InitInstance();

	AfxEnableControlContainer();

	// ��׼��ʼ��
	// ���δʹ����Щ���ܲ�ϣ����С
	// ���տ�ִ���ļ��Ĵ�С����Ӧ�Ƴ�����
	// ����Ҫ���ض���ʼ������
	// �������ڴ洢���õ�ע�����
	// TODO: Ӧ�ʵ��޸ĸ��ַ�����
	// �����޸�Ϊ��˾����֯��
	SetRegistryKey(_T("Ӧ�ó��������ɵı���Ӧ�ó���"));

    INT_PTR nResponse;

    try
    {
        CDlgGobangUI dlg;
        m_pMainWnd = &dlg;
        nResponse = dlg.DoModal();
    }
    catch(std::exception& e)
    {
        MessageBox(NULL, e.what(), "�쳣", MB_OK);
    }
	
	if (nResponse == IDOK)
	{
		// TODO: �ڴ˴����ô����ʱ�á�ȷ�������ر�
		//  �Ի���Ĵ���
	}
	else if (nResponse == IDCANCEL)
	{
		// TODO: �ڴ˷��ô����ʱ�á�ȡ�������ر�
		//  �Ի���Ĵ���
	}

	// ���ڶԻ����ѹرգ����Խ����� FALSE �Ա��˳�Ӧ�ó���
	//  ����������Ӧ�ó������Ϣ�á�
	return FALSE;
}
