// Console.cpp : �������̨Ӧ�ó������ڵ㡣
//

#include "stdafx.h"

#include <cassert>
#include <ctime>

#include <algorithm>
#include <memory>

#include <windows.h>

#define ASSERT_INSIDE_N(i, n) assert(i >= 0 && i < n)
const int NUMBER_RECT_SIZE = 40;
const int NUMBER_RECT_BORDER_W = 2;

static bool g_enableNumberSelAssist = false;
static int g_diffcult = 32;

static void Grid9In9x9_copyRow(int *destGrid, int destRow, int *srcGrid, int srcRow)
{
    ASSERT_INSIDE_N(destRow, 3);
    ASSERT_INSIDE_N(srcRow, 3);
    memcpy(destGrid + destRow * 9, srcGrid + srcRow * 9, 3 * sizeof(int));
}

static void Grid9In9x9_copyCol(int *destGrid, int destCol, int *srcGrid, int srcCol)
{
    ASSERT_INSIDE_N(destCol, 3);
    ASSERT_INSIDE_N(srcCol, 3);
    for (int i = 0; i < 3; ++i)
    {
        *(destGrid + i * 9 + destCol) = *(srcGrid + i * 9 + srcCol);
    }
}

class ShuDuLogic
{
public:
    ShuDuLogic(int removeNumberCnt)
    {
        ASSERT_INSIDE_N(removeNumberCnt, 81);
        
        // ����м䷽��
        {
            int a[9] = {0};
            for (int i = 0; i < 9; ++i) a[i] = i;
            std::random_shuffle(a, a + 9);

            memcpy(getSmallGridBase(3, 3) + 9 * 0, a + 3 * 0, 3 * sizeof(int));
            memcpy(getSmallGridBase(3, 3) + 9 * 1, a + 3 * 1, 3 * sizeof(int));
            memcpy(getSmallGridBase(3, 3) + 9 * 2, a + 3 * 2, 3 * sizeof(int));
        }

        // �������
        Grid9In9x9_copyRow(getSmallGridBase(0, 3), 2, getSmallGridBase(3, 3), 0);
        Grid9In9x9_copyRow(getSmallGridBase(0, 3), 0, getSmallGridBase(3, 3), 1);
        Grid9In9x9_copyRow(getSmallGridBase(0, 3), 1, getSmallGridBase(3, 3), 2);

        Grid9In9x9_copyRow(getSmallGridBase(6, 3), 1, getSmallGridBase(3, 3), 0);
        Grid9In9x9_copyRow(getSmallGridBase(6, 3), 2, getSmallGridBase(3, 3), 1);
        Grid9In9x9_copyRow(getSmallGridBase(6, 3), 0, getSmallGridBase(3, 3), 2);

        // �������
        Grid9In9x9_copyCol(getSmallGridBase(3, 0), 2, getSmallGridBase(3, 3), 0);
        Grid9In9x9_copyCol(getSmallGridBase(3, 0), 0, getSmallGridBase(3, 3), 1);
        Grid9In9x9_copyCol(getSmallGridBase(3, 0), 1, getSmallGridBase(3, 3), 2);

        Grid9In9x9_copyCol(getSmallGridBase(3, 6), 1, getSmallGridBase(3, 3), 0);
        Grid9In9x9_copyCol(getSmallGridBase(3, 6), 2, getSmallGridBase(3, 3), 1);
        Grid9In9x9_copyCol(getSmallGridBase(3, 6), 0, getSmallGridBase(3, 3), 2);

        // ��������Ͻ�
        Grid9In9x9_copyRow(getSmallGridBase(0, 0), 2, getSmallGridBase(3, 0), 0);
        Grid9In9x9_copyRow(getSmallGridBase(0, 0), 0, getSmallGridBase(3, 0), 1);
        Grid9In9x9_copyRow(getSmallGridBase(0, 0), 1, getSmallGridBase(3, 0), 2);

        Grid9In9x9_copyRow(getSmallGridBase(6, 0), 1, getSmallGridBase(3, 0), 0);
        Grid9In9x9_copyRow(getSmallGridBase(6, 0), 2, getSmallGridBase(3, 0), 1);
        Grid9In9x9_copyRow(getSmallGridBase(6, 0), 0, getSmallGridBase(3, 0), 2);

        // ��������½�
        Grid9In9x9_copyRow(getSmallGridBase(0, 6), 2, getSmallGridBase(3, 6), 0);
        Grid9In9x9_copyRow(getSmallGridBase(0, 6), 0, getSmallGridBase(3, 6), 1);
        Grid9In9x9_copyRow(getSmallGridBase(0, 6), 1, getSmallGridBase(3, 6), 2);

        Grid9In9x9_copyRow(getSmallGridBase(6, 6), 1, getSmallGridBase(3, 6), 0);
        Grid9In9x9_copyRow(getSmallGridBase(6, 6), 2, getSmallGridBase(3, 6), 1);
        Grid9In9x9_copyRow(getSmallGridBase(6, 6), 0, getSmallGridBase(3, 6), 2);

        // ����
        {
            // ����
            for (int y = 0; y < 9; ++y)
            {
                int existFlag = 0;
                for (int x = 0; x < 9; ++x) existFlag |= 1 << m_grid[y][x];
                assert(existFlag == 0x1ff);
            }
            // ����
            for (int x = 0; x < 9; ++x)
            {
                int existFlag = 0;
                for (int y = 0; y < 9; ++y) existFlag |= 1 << m_grid[y][x];
                assert(existFlag == 0x1ff);
            }
            // ���ӷ���
            for (int i = 0; i < 9; ++i)
            {
                int existFlag = 0;
                for (int idx = 0; idx < 9; ++idx) existFlag |= 1 << getSmallGridNumber(i % 3 * 3, i / 3 * 3, idx);
                assert(existFlag == 0x1ff);
            }
        }

        // ����Ƴ�
        {
            int a[81] = {0};
            for (int i = 0; i < 81; ++i) a[i] = i;
            std::random_shuffle(a, a + 81);

            for (int i = 0; i < removeNumberCnt; ++i)
            {
                m_grid[a[i] / 9][a[i] % 9] = -1;
            }
        }

        for (int y = 0; y < 9; ++y)
        {
            for (int x = 0; x < 9; ++x)
            {
                m_enableChange[y][x] = m_grid[y][x] == -1;
            }
        }
    }

    int getNumber(int x, int y) const
    {
        ASSERT_INSIDE_N(x, 9);
        ASSERT_INSIDE_N(y, 9);
        return m_grid[y][x];
    }
    void setNumber(int x, int y, int val)
    {
        ASSERT_INSIDE_N(x, 9);
        ASSERT_INSIDE_N(y, 9);
        assert(isEnableChange(x, y));
        assert(val == -1 || (getEnableFlag(x, y) & (1 << val)) != 0);
        m_grid[y][x] = val;
    }
    bool isEnableChange(int x, int y) const
    {
        ASSERT_INSIDE_N(x, 9);
        ASSERT_INSIDE_N(y, 9);
        return m_enableChange[y][x];
    }
    int getEnableFlag(int x, int y) const
    {
        ASSERT_INSIDE_N(x, 9);
        ASSERT_INSIDE_N(y, 9);
        if (getNumber(x, y) != -1) return 0;

        int existFlag = 0;
        for (int i = 0; i < 9; ++i)
        {
            existFlag |= 1 << getNumber(i, y);
            existFlag |= 1 << getNumber(x, i);
            existFlag |= 1 << getSmallGridNumber(x, y, i);
        }
        return 0x1ff ^ existFlag;
    }
    bool isSuccess() const
    {
        for (int y = 0; y < 9; ++y)
        {
            for (int x = 0; x < 9; ++x)
            {
                if (m_grid[y][x] == -1) return false;
            }
        }
        return true;
    }

private:
    // ȡ��x,y�����ӷ����idx�����ֵ
    int& getSmallGridNumber(int x, int y, int idx)
    {
        return (int&)((const ShuDuLogic*)this)->getSmallGridNumber(x, y, idx);
    }
    const int& getSmallGridNumber(int x, int y, int idx) const
    {
        ASSERT_INSIDE_N(x, 9);
        ASSERT_INSIDE_N(y, 9);
        ASSERT_INSIDE_N(idx, 9);
        return *(getSmallGridBase(x, y) + idx / 3 * 9 + idx % 3);
    }
    // x,yȷ���ӷ���
    int* getSmallGridBase(int x, int y) const
    {
        return (int*)&m_grid[y / 3 * 3][x / 3 * 3];
    }

private:
    int m_grid[9][9];
    bool m_enableChange[9][9];
};

class NineGoneDlg :
    public QDialog
{
public:
    NineGoneDlg(int enableFlag):
    m_selectedNumber(-1),
    m_enableFlag(enableFlag)
    {
        this->setWindowFlags(Qt::Tool);

        QGridLayout *gridLayout = new QGridLayout();
        gridLayout->setSpacing(NUMBER_RECT_BORDER_W);
        setLayout(gridLayout);

        for (int i = 0; i < 9; ++i)
        {
            QPushButton *btn = new QPushButton(QString("%0").arg(i + 1));
            btn->setEnabled(g_enableNumberSelAssist ? ((enableFlag & (1 << i)) != 0) : true);
            btn->setFixedSize(NUMBER_RECT_SIZE - NUMBER_RECT_BORDER_W, NUMBER_RECT_SIZE - NUMBER_RECT_BORDER_W);
            this->connect(btn, SIGNAL(clicked(bool)), SLOT(onClicked(bool)));

            gridLayout->addWidget(btn, i / 3, i % 3);
        }
    }

    int getSelectNumber() const
    {
        return m_selectedNumber;
    }

private:
    Q_OBJECT

private slots:
    void onClicked(bool checked)
    {
        QPushButton* btn = qobject_cast<QPushButton*>(this->focusWidget());
        if (btn == NULL) { assert(0); return; }

        int num = btn->text().toInt() - 1;
        if ((m_enableFlag & (1 << num)) != 0)
        {
            m_selectedNumber = num;
            this->done(QDialog::Accepted);
        }
        else this->done(QDialog::Rejected);
    }

private:
    int m_selectedNumber;
    int m_enableFlag;
};  

#include "moc/Console.cpp.cpp"

class MainDlg : 
    public QDialog
{
public:
    MainDlg(int n):
    m_logic(new ShuDuLogic(n))
    {
        setFixedSize(NUMBER_RECT_SIZE * 9 + NUMBER_RECT_BORDER_W * 2, NUMBER_RECT_SIZE * 9 + NUMBER_RECT_BORDER_W * 2);
    }

    virtual void paintEvent(QPaintEvent *evt)
    {
        QPainter painter(this);

        QPen blackPan = painter.pen();
        QPen grayPen2 = QPen(QColor(32, 32, 32));
        grayPen2.setWidth(2);
        QPen bluePen = QPen(QColor(0, 0, 255));

        // ��������
        {
            for (int i = 0; i < 10; ++i)
            {
                if (i % 3 == 0) painter.setPen(grayPen2);

                // ����
                painter.drawLine(
                    QPoint(NUMBER_RECT_BORDER_W + 0 * NUMBER_RECT_SIZE, NUMBER_RECT_BORDER_W + i * NUMBER_RECT_SIZE), 
                    QPoint(NUMBER_RECT_BORDER_W + 9 * NUMBER_RECT_SIZE, NUMBER_RECT_BORDER_W + i * NUMBER_RECT_SIZE));
                // ����
                painter.drawLine(
                    QPoint(NUMBER_RECT_BORDER_W + i * NUMBER_RECT_SIZE, NUMBER_RECT_BORDER_W + 0 * NUMBER_RECT_SIZE), 
                    QPoint(NUMBER_RECT_BORDER_W + i * NUMBER_RECT_SIZE, NUMBER_RECT_BORDER_W + 9 * NUMBER_RECT_SIZE));

                if (i % 3 == 0) painter.setPen(blackPan);
            }
        }
        // ��������
        const int INNER_SIZE = NUMBER_RECT_SIZE - 2 * NUMBER_RECT_BORDER_W;
        {
            painter.setFont(QFont("����", NUMBER_RECT_SIZE - 6 * NUMBER_RECT_BORDER_W, 75));

            for (int y = 0; y < 9; ++y)
            {
                for (int x = 0; x < 9; ++x)
                {
                    const int posX = NUMBER_RECT_BORDER_W + NUMBER_RECT_SIZE * x + NUMBER_RECT_BORDER_W;
                    const int posY = NUMBER_RECT_BORDER_W + NUMBER_RECT_SIZE * y + NUMBER_RECT_BORDER_W;

                    int n = m_logic->getNumber(x, y);
                    if (n != -1)
                    {
                        painter.drawRect(posX, posY, INNER_SIZE, INNER_SIZE);

                        const int X_OFFSET = 8, Y_OFFSET = -4;
                        if (m_logic->isEnableChange(x, y)) painter.setPen(bluePen);
                        painter.drawText(posX + X_OFFSET, posY + INNER_SIZE + Y_OFFSET, QString("%0").arg(n + 1));
                        if (m_logic->isEnableChange(x, y)) painter.setPen(blackPan);
                    }
                }
            }
        }
        // ����ѡ�и�
        if (m_selPos.get() != NULL)
        {
            const int posX = NUMBER_RECT_BORDER_W + NUMBER_RECT_SIZE * m_selPos->x() + NUMBER_RECT_BORDER_W;
            const int posY = NUMBER_RECT_BORDER_W + NUMBER_RECT_SIZE * m_selPos->y() + NUMBER_RECT_BORDER_W;

            painter.setPen(grayPen2);
            painter.drawRect(posX, posY, INNER_SIZE, INNER_SIZE);    
            painter.setPen(blackPan);
        }

        QDialog::paintEvent(evt);
    }

    virtual void mouseReleaseEvent(QMouseEvent *evt)
    {
        QDialog::mouseReleaseEvent(evt);

        int x = (evt->x() - NUMBER_RECT_BORDER_W) / NUMBER_RECT_SIZE;
        int y = (evt->y() - NUMBER_RECT_BORDER_W) / NUMBER_RECT_SIZE;
        if (x < 0 || x >= 9) return;
        if (y < 0 || y >= 9) return;

        if (!m_logic->isEnableChange(x, y)) return;

        if (evt->button() == Qt::LeftButton)
        {
            int flag = m_logic->getEnableFlag(x, y);
            if (flag == 0) return;

            m_selPos.reset(new QPoint(x, y));

            NineGoneDlg dlg(flag);
            dlg.setWindowTitle("ѡ������");
            if (dlg.exec() == QDialog::Accepted)
            {
                m_logic->setNumber(x, y, dlg.getSelectNumber());
                this->update();

                if (m_logic->isSuccess())
                {
                    QMessageBox msgBox(QMessageBox::Information, "��Ϸ����", "��ɹ����������Ϸ��");
                    msgBox.setButtonText(QMessageBox::Ok, "ȷ��");
                    msgBox.exec();
                }
            }

            m_selPos.reset();
        }
        else
        {
            m_logic->setNumber(x, y, -1);
            this->update();
        }
    }
    virtual void keyReleaseEvent(QKeyEvent * evt)
    {
        QDialog::keyReleaseEvent(evt);

        if (evt->key() == Qt::Key_F1) 
        {
            QMessageBox msgBox(QMessageBox::Information, "���� by Scan", 
                "���   : ѡ�����֡�\n"
                "�Ҽ�   : ������֡�\n"
                "F2     : �����Ѷȡ�\n"
                "F3     : �л����ָ���ѡ�񿪹ء�\n");
            msgBox.setButtonText(QMessageBox::Ok, "ȷ��");
            msgBox.exec();
        }
        else if (evt->key() == Qt::Key_F2)
        {
            QInputDialog dlg;
            dlg.setWindowTitle("�л��Ѷ�");
            dlg.setLabelText("�����Ѷ�ϵ����0~64����");
            dlg.setOkButtonText("ȷ��");
            dlg.setCancelButtonText("ȡ��");
            dlg.setInputMode(QInputDialog::IntInput);
            dlg.setIntRange(0, 64);
            dlg.setIntValue(g_diffcult);
            if (dlg.exec() == QDialog::Accepted)
            {
                g_diffcult = dlg.intValue();
                WinExec(
                    (const char*)QString("%0 %1 %2").
                    arg(QApplication::instance()->argv()[0]).
                    arg(g_diffcult).
                    arg(g_enableNumberSelAssist ? "true" : "false").
                    toLocal8Bit(), 
                    SW_SHOW);

                this->close();
            }
        }
        else if (evt->key() == Qt::Key_F3)
        {
            QMessageBox msgBox(
                QMessageBox::Question,
                "���ָ���ѡ�񿪹�", 
                g_enableNumberSelAssist ? "�رո���������" : "��������������", 
                QMessageBox::Yes | QMessageBox::No);
            msgBox.setButtonText(QMessageBox::Yes, "��");
            msgBox.setButtonText(QMessageBox::No, "��");
            if (msgBox.exec() == QMessageBox::Yes)
            {
                g_enableNumberSelAssist = !g_enableNumberSelAssist;
                WinExec(
                    (const char*)QString("%0 %1 %2").
                    arg(QApplication::instance()->argv()[0]).
                    arg(g_diffcult).
                    arg(g_enableNumberSelAssist ? "true" : "false").
                    toLocal8Bit(), 
                    SW_SHOW);

                this->close();
            }
        }
    }

private:
    std::auto_ptr<ShuDuLogic>   m_logic;
    std::auto_ptr<QPoint>       m_selPos;
};

int main(int argc, char *argv[])
{
    if (argc > 1) g_diffcult = QString(argv[1]).toInt();
    if (argc > 2) g_enableNumberSelAssist = QString(argv[2]) == "true";

    srand(time(NULL));

    QApplication app(argc, argv);

	// ����û�а�װqt�������������û���Ϊ�˱������룬��Ҫ�����¹�����
	// ��ִ���ļ�Ŀ¼�½�����Ŀ¼������ļ���plugin/codecs/qcncodecs4.dll��
	// ����Ϊgb18030�����ʵ��λ��qcncodecs4.dll�У�
    QApplication::addLibraryPath(QApplication::applicationDirPath() + "/plugin");
    QTextCodec::setCodecForCStrings(QTextCodec::codecForName("gb18030"));

    MainDlg dlg(g_diffcult);
    dlg.setWindowTitle("����    F1-����");
    dlg.show();

    app.exec();
}

int WINAPI WinMain( __in HINSTANCE hInstance, __in_opt HINSTANCE hPrevInstance, __in_opt LPSTR lpCmdLine, __in int nShowCmd )
{
    return main(__argc, __argv);
}