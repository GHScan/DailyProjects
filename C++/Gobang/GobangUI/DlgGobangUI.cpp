// DlgGobangUI.cpp : 实现文件
//

#include "stdafx.h"

#include <ctime>

#include "GobangUI.h"
#include "DlgGobangUI.h"

#if defined(_DEBUG)
#define DLL_POST "Debug"
#else
#define DLL_POST "Release"
#endif

#include "../Gobang/IPlayer.h"
#pragma comment(lib, "../bin/gobang_" DLL_POST)

#ifdef _DEBUG
#define new DEBUG_NEW
#endif


// 用于应用程序“关于”菜单项的 CAboutDlg 对话框
class HumanPlayer:
    public IPlayer
{
public:
    HumanPlayer(){}

    virtual void init(int size, HWND wnd)
    {

    }

    virtual void destroy()
    {
        delete this;
    }

    virtual void reset()
    {
    }

    virtual void notifyPlaced(int x, int y)
    {
    }

    virtual bool requirePlace(int *x, int *y)
    {
        if (x == NULL)
        {
            m_isWait = true;
            return false;
        }
        else
        {
            m_isWait = false;
            *x = m_x, *y = m_y;
            return true;
        }
    }

    virtual const char* getName()
    {
        return "玩家";
    }

    void setXY(int x, int y)
    {
        m_x = x, m_y = y;
    }

    bool isWait() const
    {
        return m_isWait;
    }

    virtual bool canPeerWin() const
    {
        return m_peer->canWin();
    }

    virtual bool canWin() const
    {
        return m_peer->canPeerWin();
    }

    void setPeer(IPlayer *peer)
    {
        m_peer = peer;
    }

private:
    int     m_x, m_y;
    bool    m_isWait;
    IPlayer *m_peer;
};

class CAboutDlg : 
    public CDialog
{
public:
	CAboutDlg();

// 对话框数据
	enum { IDD = IDD_ABOUTBOX };

	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV 支持

// 实现
protected:
	DECLARE_MESSAGE_MAP()
};

CAboutDlg::CAboutDlg() : CDialog(CAboutDlg::IDD)
{
}

void CAboutDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
}

BEGIN_MESSAGE_MAP(CAboutDlg, CDialog)
END_MESSAGE_MAP()
// CDlgGobangUI 对话框

class CDlgChessboard:
    public CDialog
{
public:
    enum { IDD = IDD_DLG_CHESSBOARD };

    DECLARE_MESSAGE_MAP()

protected:
    afx_msg BOOL OnEraseBkgnd(CDC* pDC)
    {
        CRect rt;
        GetClientRect(&rt);
        static_cast<CDlgGobangUI*>(GetParent())->onChessboardPaint(pDC, &rt);

        return TRUE;
    }

    afx_msg void OnLButtonUp(
        UINT nFlags,
        CPoint point)
    {
        CRect rt;
        GetClientRect(&rt);
        static_cast<CDlgGobangUI*>(GetParent())->onChessboardLButtonUp(
            point.x / float(rt.Width()), point.y / float(rt.Height()));
    }

protected:

};

BEGIN_MESSAGE_MAP(CDlgChessboard, CDialog)
    //}}AFX_MSG_MAP
    ON_WM_ERASEBKGND()
    ON_WM_LBUTTONUP()
END_MESSAGE_MAP()

CDlgGobangUI::CDlgGobangUI(CWnd* pParent /*=NULL*/)
	: 
    CDialog(CDlgGobangUI::IDD, pParent),
    m_chessBoard(NULL),
    m_pc0(NULL),
    m_pc1(NULL),
    m_memDC(NULL),
    m_lastX(0),
    m_lastY(0),
    m_lastMoveTime(0)
{
	m_hIcon = AfxGetApp()->LoadIcon(IDR_MAINFRAME);

    ZeroMemory(m_boardData, sizeof(m_boardData));

    srand(unsigned(time(NULL)));
}

void CDlgGobangUI::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
}

BEGIN_MESSAGE_MAP(CDlgGobangUI, CDialog)
	ON_WM_SYSCOMMAND()
	ON_WM_PAINT()
	ON_WM_QUERYDRAGICON()
	//}}AFX_MSG_MAP
    ON_BN_CLICKED(IDC_BTN_PVC, &CDlgGobangUI::OnBnClickedBtnPvc)
    ON_BN_CLICKED(IDC_BTN_CVC, &CDlgGobangUI::OnBnClickedBtnCvc)
    ON_MESSAGE(UM_PCPLAYER_THINKOVER, &CDlgGobangUI::onPlayerThinkOverMsg)
    ON_CBN_SELCHANGE(IDC_COMBO_PC0, &CDlgGobangUI::OnCbnSelchangeComboPc0)
    ON_CBN_SELCHANGE(IDC_COMBO_PC1, &CDlgGobangUI::OnCbnSelchangeComboPc1)
END_MESSAGE_MAP()


// CDlgGobangUI 消息处理程序

void CDlgGobangUI::OnSysCommand(UINT nID, LPARAM lParam)
{
	if ((nID & 0xFFF0) == IDM_ABOUTBOX)
	{
		CAboutDlg dlgAbout;
		dlgAbout.DoModal();
	}
	else
	{
		CDialog::OnSysCommand(nID, lParam);
	}
}

// 如果向对话框添加最小化按钮，则需要下面的代码
//  来绘制该图标。对于使用文档/视图模型的 MFC 应用程序，
//  这将由框架自动完成。

void CDlgGobangUI::OnPaint()
{
	if (IsIconic())
	{
		CPaintDC dc(this); // 用于绘制的设备上下文

		SendMessage(WM_ICONERASEBKGND, reinterpret_cast<WPARAM>(dc.GetSafeHdc()), 0);

		// 使图标在工作矩形中居中
		int cxIcon = GetSystemMetrics(SM_CXICON);
		int cyIcon = GetSystemMetrics(SM_CYICON);
		CRect rect;
		GetClientRect(&rect);
		int x = (rect.Width() - cxIcon + 1) / 2;
		int y = (rect.Height() - cyIcon + 1) / 2;

		// 绘制图标
		dc.DrawIcon(x, y, m_hIcon);
	}
	else
	{
		CDialog::OnPaint();
	}
}
//当用户拖动最小化窗口时系统调用此函数取得光标显示。
//
HCURSOR CDlgGobangUI::OnQueryDragIcon()
{
	return static_cast<HCURSOR>(m_hIcon);
}

BOOL CDlgGobangUI::OnInitDialog()
{
    CDialog::OnInitDialog();

    // 将“关于...”菜单项添加到系统菜单中。

    // IDM_ABOUTBOX 必须在系统命令范围内。
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

    // 设置此对话框的图标。当应用程序主窗口不是对话框时，框架将自动
    //  执行此操作
    SetIcon(m_hIcon, TRUE);			// 设置大图标
    SetIcon(m_hIcon, FALSE);		// 设置小图标

    // TODO: 在此添加额外的初始化代码
    m_chessBoard = new CDlgChessboard;
    m_chessBoard->Create(CDlgChessboard::IDD, this);
    m_chessBoard->MoveWindow(120, 25, 400, 400);
    m_chessBoard->ShowWindow(SW_SHOW);

    std::vector<std::string> ais;
    IPlayer_getAIList(ais);

    CComboBox* combo0 = (CComboBox*)GetDlgItem(IDC_COMBO_PC0);
    CComboBox* combo1 = (CComboBox*)GetDlgItem(IDC_COMBO_PC1);
    for (size_t i = 0; i < ais.size(); ++i)
    {   
        combo0->AddString(ais[i].c_str());
        combo1->AddString(ais[i].c_str());
    }
    combo0->SetCurSel(0);
    combo1->SetCurSel(0);

    m_pc0 = IPlayer_create(ais[0].c_str(), ais[0].c_str());
    m_pc0->init(BOARD_SIZE, GetSafeHwnd());

    m_pc1 = IPlayer_create(ais[0].c_str(), ais[0].c_str());
    m_pc1->init(BOARD_SIZE, GetSafeHwnd());

    m_player = new HumanPlayer;
    m_player->init(BOARD_SIZE, GetSafeHwnd());

    OnBnClickedBtnPvc();

    return TRUE;  // 除非将焦点设置到控件，否则返回 TRUE
}

BOOL CDlgGobangUI::DestroyWindow()
{   
    m_memDC->DeleteDC();
    delete m_memDC;
    m_memDC = NULL;

    m_player->destroy();
    m_player = NULL;

    m_pc0->destroy();
    m_pc0 = NULL;

    m_pc1->destroy();
    m_pc1 = NULL;

    m_chessBoard->DestroyWindow();
    delete m_chessBoard;
    m_chessBoard = NULL;

    return CDialog::DestroyWindow();
}


void CDlgGobangUI::onChessboardPaint(CDC *dc, CRect *rect)
{   
    dc->FillRect(rect, CBrush::FromHandle((HBRUSH)GetStockObject(GRAY_BRUSH)));

    int w = int(rect->Width() / float(BOARD_SIZE));
    int h = int(rect->Height() / float(BOARD_SIZE));

    CBrush *brush[2] = 
    {
        CBrush::FromHandle((HBRUSH)GetStockObject(BLACK_BRUSH)),
        CBrush::FromHandle((HBRUSH)GetStockObject(WHITE_BRUSH)),
    };

    if (m_memDC == NULL)
    {
        m_memDC = new CDC;
        m_memDC->CreateCompatibleDC(dc);
        CBitmap bm;
        bm.CreateCompatibleBitmap(dc, rect->Width(), rect->Height());
        m_memDC->SelectObject(&bm);
        bm.Detach();
    }

    m_memDC->FillSolidRect(0, 0, rect->Width(), rect->Height(), RGB(128, 128, 128));

    m_memDC->SelectObject(brush[0]);
    for (int i = 0; i <= BOARD_SIZE; ++i)
    {
        m_memDC->MoveTo(CPoint(i * w, 0));
        m_memDC->LineTo(CPoint(i * w, rect->Height()));

        m_memDC->MoveTo(CPoint(0, i * w));
        m_memDC->LineTo(CPoint(rect->Height(), i * w));
    }

    for (int i = 0; i < BOARD_SIZE; ++i)
    {
        for (int j = 0; j < BOARD_SIZE; ++j)
        {
            CRect rt(w * i, h * j, w * i + w, h * j + h);

            int side = m_boardData[j][i];
            if (side != 0)
            {
                m_memDC->SelectObject(brush[side - 1]);
                m_memDC->Ellipse(rt);
            }
        }
    }

    m_memDC->Draw3dRect(
        CRect(m_lastX * w, m_lastY * h, m_lastX * w + w, m_lastY * h + h), 
        RGB(255, 0, 0), RGB(255, 0, 0));

    dc->BitBlt(0, 0, rect->Width(), rect->Height(), m_memDC, 0, 0, SRCCOPY);
}

void CDlgGobangUI::OnBnClickedBtnPvc()
{
    // TODO: 在此添加控件通知处理程序代码
    m_peer[0] = m_player;
    m_peer[1] = m_pc0;

    ((HumanPlayer*)m_player)->setPeer(m_pc0);

    reset();
}

void CDlgGobangUI::OnBnClickedBtnCvc()
{
    // TODO: 在此添加控件通知处理程序代码
    m_peer[0] = m_pc0;
    m_peer[1] = m_pc1;

    reset();
}

void CDlgGobangUI::reset()
{
    m_nowSide = 0;

    m_peer[0]->reset();
    m_peer[1]->reset();

    ZeroMemory(m_boardData, sizeof(m_boardData));

    m_peer[0]->requirePlace();

    m_chessBoard->RedrawWindow();
}

LRESULT CDlgGobangUI::onPlayerThinkOverMsg(WPARAM, LPARAM)
{
    const int WAIT_TIME = 1;
    DWORD time = GetTickCount();
    if (time - m_lastMoveTime < WAIT_TIME)
    {
        Sleep(WAIT_TIME - (time - m_lastMoveTime));
    }
    m_lastMoveTime = GetTickCount();

    int x, y;

    if (!m_peer[m_nowSide]->requirePlace(&x, &y))
    {
        m_chessBoard->RedrawWindow();
        MessageBox("和棋", "游戏结束", MB_OK);
        return 1;
    }

    m_nowSide = 1 - m_nowSide;
    m_peer[m_nowSide]->notifyPlaced(x, y);

    m_boardData[y][x] = (1 - m_nowSide) + 1;
    m_lastX = x, m_lastY = y;
    m_chessBoard->Invalidate();

    if (m_peer[m_nowSide]->canPeerWin())
    {
        m_chessBoard->RedrawWindow();
        MessageBox(CString(m_peer[1 - m_nowSide]->getName()) + "胜利", "游戏结束", MB_OK);
        return 1;
    }

    m_peer[m_nowSide]->requirePlace();

    return 1;
}

void CDlgGobangUI::onChessboardLButtonUp(float x, float y)
{
    if (HumanPlayer *p = dynamic_cast<HumanPlayer*>(m_peer[0]))
    {
        if (p->isWait())
        {
            int _x = int(x * BOARD_SIZE), _y = int(y * BOARD_SIZE);

            if (m_boardData[_y][_x] == 0)
            {
                p->setXY(_x, _y);
                PostMessage(UM_PCPLAYER_THINKOVER, 0, 0);
            }
        }
    }
}

void CDlgGobangUI::OnCbnSelchangeComboPc0()
{
    CComboBox *combo = (CComboBox*)GetDlgItem(IDC_COMBO_PC0);

    if (combo->GetCurSel() == -1)
    {
        return;
    }

    CString s;
    combo->GetLBText(combo->GetCurSel(), s);
    
    m_pc0->destroy();
    m_pc0 = IPlayer_create(s.GetString(), s.GetString());
    m_pc0->init(BOARD_SIZE, GetSafeHwnd());
}

void CDlgGobangUI::OnCbnSelchangeComboPc1()
{
    CComboBox *combo = (CComboBox*)GetDlgItem(IDC_COMBO_PC1);

    if (combo->GetCurSel() == -1)
    {
        return;
    }

    CString s;
    combo->GetLBText(combo->GetCurSel(), s);

    m_pc1->destroy();
    m_pc1 = IPlayer_create(s.GetString(), s.GetString());
    m_pc1->init(BOARD_SIZE, GetSafeHwnd());
}
