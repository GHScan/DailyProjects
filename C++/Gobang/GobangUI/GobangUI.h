// GobangUI.h : PROJECT_NAME Ӧ�ó������ͷ�ļ�
//

#pragma once

#ifndef __AFXWIN_H__
	#error "�ڰ������ļ�֮ǰ������stdafx.h�������� PCH �ļ�"
#endif

#include "resource.h"		// ������


// CAppGobangUI:
// �йش����ʵ�֣������ GobangUI.cpp
//

class CAppGobangUI : 
    public CWinApp
{
public:
	CAppGobangUI();

// ��д
	public:
	virtual BOOL InitInstance();

// ʵ��

	DECLARE_MESSAGE_MAP()
};

extern CAppGobangUI theApp;