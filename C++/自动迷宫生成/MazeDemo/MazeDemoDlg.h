// MazeDemoDlg.h : 头文件
//
#pragma once

class DisjointUnion;

#include <vector>
// CMazeDemoDlg 对话框
class CMazeDemoDlg : public CDialog
{
// 构造
public:
	CMazeDemoDlg(CWnd* pParent = NULL);	// 标准构造函数

// 对话框数据
	enum { IDD = IDD_MAZEDEMO_DIALOG };

	protected:
	virtual void DoDataExchange(CDataExchange* pDX);	// DDX/DDV 支持


// 实现
protected:
	HICON m_hIcon;

	// 生成的消息映射函数
public:

protected:
    DECLARE_MESSAGE_MAP()

    afx_msg void OnSysCommand(UINT nID, LPARAM lParam);
    afx_msg void OnPaint();
    afx_msg HCURSOR OnQueryDragIcon();
    afx_msg void OnSize(UINT nType, int cx, int cy);

protected:
    virtual BOOL OnInitDialog();

private:
    void Reset();
    void findPath(DisjointUnion& dSet, std::vector<int>& vResult, std::vector<int>& vTry);
    void getNextPos(int iCur, std::vector<int>& vNext);

private:
    int                             m_iWidth;
    int                             m_iHeight;
    int                             m_iGrid;
    
    std::vector<int>                m_vMap;
    CBitmap                         m_bmMem;
    afx_msg void OnKeyUp(UINT nChar, UINT nRepCnt, UINT nFlags);
};
