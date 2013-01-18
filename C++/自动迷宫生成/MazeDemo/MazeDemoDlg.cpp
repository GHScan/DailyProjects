// MazeDemoDlg.cpp : ʵ���ļ�
//

#include "stdafx.h"
#include "MazeDemo.h"
#include "MazeDemoDlg.h"
#include "DisjointUnion.h"

#include <boost/unordered_set.hpp>
#include <ctime>

#ifdef _DEBUG
#define new DEBUG_NEW
#endif
// ����Ӧ�ó��򡰹��ڡ��˵���� CAboutDlg �Ի���

class CAboutDlg : public CDialog
{
public:
    CAboutDlg(int i);
    int getGrid() { return m_iGrid; }
// �Ի�������
	enum { IDD = IDD_ABOUTBOX };

	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV ֧��

// ʵ��
protected:
	DECLARE_MESSAGE_MAP()
private:
    int m_iGrid;
};

CAboutDlg::CAboutDlg(int i) : CDialog(CAboutDlg::IDD)
, m_iGrid(i)
{
}

void CAboutDlg::DoDataExchange(CDataExchange* pDX)
{
    CDialog::DoDataExchange(pDX);
    DDX_Text(pDX, IDC_EDIT1, m_iGrid);
	DDV_MinMaxInt(pDX, m_iGrid, 3, 99);
}

BEGIN_MESSAGE_MAP(CAboutDlg, CDialog)
END_MESSAGE_MAP()


// CMazeDemoDlg �Ի���




CMazeDemoDlg::CMazeDemoDlg(CWnd* pParent /*=NULL*/)
	: 
    CDialog(CMazeDemoDlg::IDD, pParent),
    m_iGrid(8)
{
	m_hIcon = AfxGetApp()->LoadIcon(IDR_MAINFRAME);
}

void CMazeDemoDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
}

BEGIN_MESSAGE_MAP(CMazeDemoDlg, CDialog)
	ON_WM_SYSCOMMAND()
	ON_WM_PAINT()
	ON_WM_QUERYDRAGICON()
	//}}AFX_MSG_MAP

    ON_WM_SIZE()
    ON_WM_KEYUP()
END_MESSAGE_MAP()


// CMazeDemoDlg ��Ϣ�������

BOOL CMazeDemoDlg::OnInitDialog()
{
	CDialog::OnInitDialog();

	// ��������...���˵�����ӵ�ϵͳ�˵��С�

	// IDM_ABOUTBOX ������ϵͳ���Χ�ڡ�
	ASSERT((IDM_ABOUTBOX & 0xFFF0) == IDM_ABOUTBOX);
	ASSERT(IDM_ABOUTBOX < 0xF000);

	CMenu* pSysMenu = GetSystemMenu(FALSE);
	if (pSysMenu != NULL)
	{
		CString strAboutMenu;
		strAboutMenu.LoadString(IDS_ABOUTBOX);
		if (!strAboutMenu.IsEmpty())
		{
			pSysMenu->AppendMenu(MF_SEPARATOR);
			pSysMenu->AppendMenu(MF_STRING, IDM_ABOUTBOX, strAboutMenu);
		}
	}

	// ���ô˶Ի����ͼ�ꡣ��Ӧ�ó��������ڲ��ǶԻ���ʱ����ܽ��Զ�
	//  ִ�д˲���
	SetIcon(m_hIcon, TRUE);			// ���ô�ͼ��
	SetIcon(m_hIcon, FALSE);		// ����Сͼ��

	// TODO: �ڴ���Ӷ���ĳ�ʼ������

	return TRUE;  // ���ǽ��������õ��ؼ������򷵻� TRUE
}

void CMazeDemoDlg::OnSysCommand(UINT nID, LPARAM lParam)
{
	if ((nID & 0xFFF0) == IDM_ABOUTBOX)
	{
		CAboutDlg dlgAbout(m_iGrid);
		dlgAbout.DoModal();
        int iGrid = dlgAbout.getGrid();
        if (iGrid != m_iGrid)
        {
            m_iGrid = iGrid;
            Reset();
        }
	}
	else
	{
		CDialog::OnSysCommand(nID, lParam);
	}
}

// �����Ի��������С����ť������Ҫ����Ĵ���
//  �����Ƹ�ͼ�ꡣ����ʹ���ĵ�/��ͼģ�͵� MFC Ӧ�ó���
//  �⽫�ɿ���Զ���ɡ�

void CMazeDemoDlg::OnPaint()
{
    CPaintDC dc(this); // ���ڻ��Ƶ��豸������
    CRect rect;
    GetClientRect(&rect);

	if (IsIconic())
	{
		SendMessage(WM_ICONERASEBKGND, reinterpret_cast<WPARAM>(dc.GetSafeHdc()), 0);

		// ʹͼ���ڹ��������о���
		int cxIcon = GetSystemMetrics(SM_CXICON);
		int cyIcon = GetSystemMetrics(SM_CYICON);
		
		int x = (rect.Width() - cxIcon + 1) / 2;
		int y = (rect.Height() - cyIcon + 1) / 2;

		// ����ͼ��
		dc.DrawIcon(x, y, m_hIcon);

        return;
	}
	
    if (m_bmMem.GetSafeHandle() != NULL)
    {
        CDC cdc;
        cdc.CreateCompatibleDC(&dc);
        CBitmap *pBmOld = cdc.SelectObject(&m_bmMem);
        dc.BitBlt(0, 0, rect.Width(), rect.Height(), &cdc, 0, 0, SRCCOPY);
        cdc.SelectObject(pBmOld);
    }
}

//���û��϶���С������ʱϵͳ���ô˺���ȡ�ù����ʾ��
//
HCURSOR CMazeDemoDlg::OnQueryDragIcon()
{
	return static_cast<HCURSOR>(m_hIcon);
}
void CMazeDemoDlg::OnSize(UINT nType, int cx, int cy)
{
    if (cx > 0 && cy > 0)
    {
        Reset();
    }

    CDialog::OnSize(nType, cx, cy);
}

bool sRand()
{
    srand(unsigned(time(0)));
    return true;
}

inline int getRandom(int iUpperBound)
{
    static bool b = sRand();
    return int(rand() + GetTickCount()) % iUpperBound;
}

void CMazeDemoDlg::Reset()
{
    CRect rtClient;
    GetClientRect(&rtClient);
    m_iWidth    = rtClient.Width() / m_iGrid;
    m_iHeight   = rtClient.Height() / m_iGrid;
    if (m_iWidth <= 1 || m_iHeight <= 1)
    {
        return;
    }
    CString sz;
    sz.Format("�Թ����Ի� - %5d x %-5d", m_iWidth, m_iHeight);
    SetWindowText(sz);

    m_vMap.clear();
    m_vMap.resize(m_iWidth * m_iHeight, 0);
    DisjointUnion dSet(int(m_vMap.size()));
    while (!dSet.isJoint(0, dSet.size() - 1))
    {
        int iLine       = getRandom(m_iWidth);
        bool bLastLine  = iLine == m_iWidth - 1;
        int iRow        = getRandom(m_iHeight - (bLastLine ? 1 : 0));
        bool bLastRow   = iRow == m_iHeight - 1;
        bool bRight     = bLastRow || (!bLastLine && getRandom(2) == 0);

        int iPos    = iRow * m_iWidth + iLine;
        int iNext   = iPos + (bRight ? 1 : m_iWidth);

        if (!dSet.isJoint(iPos, iNext))
        {
            dSet.makeJoint(iPos, iNext);
            m_vMap[iPos] |= bRight ? 1 : 2;
        }
    }

    CDC *pDC = GetDC();
    CDC dc;
    dc.CreateCompatibleDC(pDC);
    m_bmMem.DeleteObject();
    m_bmMem.CreateCompatibleBitmap(pDC, rtClient.Width(), rtClient.Height());
    CBitmap *pBmOld = dc.SelectObject(&m_bmMem);
    dc.FillSolidRect(&rtClient, RGB(255, 255, 255));

    for (int i = 0; i < int(m_vMap.size()); ++i)
    {
        int v       = m_vMap[i];
        int iRow    = i / m_iWidth;
        int iLine   = i % m_iWidth;

        if ((v & 1) == 0)
        {
            dc.MoveTo((iLine + 1) * m_iGrid, (iRow + 0) * m_iGrid);
            dc.LineTo((iLine + 1) * m_iGrid, (iRow + 1) * m_iGrid);
        }   
        if ((v & 2) == 0)
        {
            dc.MoveTo((iLine + 0) * m_iGrid, (iRow + 1) * m_iGrid);
            dc.LineTo((iLine + 1) * m_iGrid, (iRow + 1) * m_iGrid);
        }
    }

    int iFrame = 1;
    std::vector<int> vExitMaze;
    std::vector<int> vTry;
    findPath(dSet, vExitMaze, vTry);
    for (int i = 0; i < int(vExitMaze.size()); ++i)
    {
        int v       = vExitMaze[i];
        int iRow    = v / m_iWidth;
        int iLine   = v % m_iWidth;

        dc.FillSolidRect(iLine * m_iGrid + iFrame, iRow * m_iGrid + iFrame, m_iGrid - 2 * iFrame,  m_iGrid - 2 * iFrame, RGB(255, 0, 0));
    }
    for (int i = 0; i < int(vTry.size()); ++i)
    {
        int v       = vTry[i];
        int iRow    = v / m_iWidth;
        int iLine   = v % m_iWidth;

        dc.FillSolidRect(iLine * m_iGrid + iFrame, iRow * m_iGrid + iFrame, m_iGrid - 2 * iFrame, m_iGrid - 2 * iFrame, RGB(128, 128, 128));
    }

    dc.SelectObject(pBmOld);
    ReleaseDC(pDC);
    RedrawWindow();
}

void CMazeDemoDlg::findPath(DisjointUnion& dSet, std::vector<int>& vResult, std::vector<int>& vTry)
{
    boost::unordered_set<int> stBlack;
    vResult.push_back(0);
    vResult.push_back(0);
    int iLastBack = -1;
    std::vector<int> vNext;
    
    while (vResult.back() != m_vMap.size() - 1)
    {
        getNextPos(vResult.back(), vNext);
        int i = 0;
        for (; i < int(vNext.size()); ++i)
        {
            if (vNext[i] != vResult[vResult.size() - 2] && 
                dSet.isJoint(0, vNext[i]) &&
                stBlack.count(vNext[i]) == 0 &&
                vNext[i] != iLastBack)
            {
                break;
            }
        }

        if (iLastBack != -1)
        {
            if (i == vNext.size())
            {
                iLastBack = vResult.back();
                vTry.push_back(iLastBack);
                vResult.pop_back();
            }
            else
            {
                stBlack.insert(iLastBack);
                iLastBack = -1;
                vResult.push_back(vNext[i]);
            }
        }
        else
        {
            if (i == vNext.size())
            {
                iLastBack = vResult.back();
                vTry.push_back(iLastBack);
                vResult.pop_back();
            }
            else
            {
                vResult.push_back(vNext[i]);
            }
        }
    }
}

void CMazeDemoDlg::getNextPos(int iCur, std::vector<int>& vNext)
{
    vNext.clear();
    int iRow    = iCur / m_iWidth;
    int iLine   = iCur % m_iWidth;
    if (iRow < m_iHeight - 1 && (m_vMap[iCur] & 2) != 0)
    {
        vNext.push_back(iCur + m_iWidth);
    }
    if (iLine < m_iWidth - 1 && (m_vMap[iCur] & 1) != 0)
    {
        vNext.push_back(iCur + 1);
    }
    if (iRow > 0 && (m_vMap[iCur - m_iWidth] & 2) != 0)
    {
        vNext.push_back(iCur - m_iWidth);
    }
    if (iLine > 0 && (m_vMap[iCur - 1] & 1) != 0)
    {
        vNext.push_back(iCur - 1);
    }
}
void CMazeDemoDlg::OnKeyUp(UINT nChar, UINT nRepCnt, UINT nFlags)
{
    switch (nChar)
    {
    case VK_DOWN:
    case VK_UP:
        if (nChar == VK_DOWN && m_iGrid > 3)
        {
            --m_iGrid;
        }
        if (nChar == VK_UP && m_iGrid < 99)
        {
            ++m_iGrid;
        }
    case VK_SPACE:
        Reset();
        break;
    default:
        break;
    }

    CDialog::OnKeyUp(nChar, nRepCnt, nFlags);
}
