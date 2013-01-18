// DlgGobangUI.h : 头文件
//

#pragma once

struct IPlayer;
// CDlgGobangUI 对话框
class CDlgGobangUI : 
    public CDialog
{
// 构造
public:
	CDlgGobangUI(CWnd* pParent = NULL);	// 标准构造函数

// 对话框数据
	enum { IDD = IDD_GOBANGUI_DIALOG };

public:
    void onChessboardPaint(CDC *dc, CRect *rect);
    void onChessboardLButtonUp(float x, float y);

protected:
    afx_msg void OnSysCommand(UINT nID, LPARAM lParam);
    afx_msg void OnPaint();
    afx_msg HCURSOR OnQueryDragIcon();
    afx_msg void OnBnClickedBtnPvc();
    afx_msg void OnBnClickedBtnCvc();
    afx_msg LRESULT onPlayerThinkOverMsg(WPARAM, LPARAM);
    afx_msg void OnCbnSelchangeComboPc0();
    afx_msg void OnCbnSelchangeComboPc1();

    DECLARE_MESSAGE_MAP()
// 实现
protected:
    // 生成的消息映射函数
    virtual BOOL OnInitDialog();	
    virtual BOOL DestroyWindow();
    virtual void DoDataExchange(CDataExchange* pDX);	// DDX/DDV 支持

private:
    void reset();

private:
    static const int BOARD_SIZE = 16;

    HICON        m_hIcon;

    CDialog     *m_chessBoard;

    char         m_boardData[BOARD_SIZE][BOARD_SIZE];
    int         m_lastX, m_lastY;
    IPlayer     *m_pc0;
    IPlayer     *m_pc1;
    IPlayer     *m_player;

    IPlayer     *m_peer[2];
    int          m_nowSide;

    CDC         *m_memDC;

    DWORD        m_lastMoveTime;
};
