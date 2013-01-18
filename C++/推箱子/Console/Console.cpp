// Console.cpp : 定义控制台应用程序的入口点。
//

#include "stdafx.h"

#include <io.h>

#include <fstream>

#include "TXZMapDefine.h"

const int MIN_MAP_WH = 3;
const int INIT_BEST_MOVE = 99999;
#define TXZMAPDB_FILENAME       "TXZMapDB.txt"

static void createNewApp(int appType)
{
    QString cmd = QApplication::instance()->argv()[0];
    switch (appType)
    {
    case Qt::Key_F2:
        break;
    case Qt::Key_F3:
        cmd += " edit";
        break; 
    case Qt::Key_F4:
        cmd += " replay";
        break;
    default:
        break;
    }
    WinExec((const char*)cmd.toLocal8Bit(), SW_SHOW);
}

static bool execInputDialog_Int(
    const char *title, const char *label, int minVal, int maxVal, int &ret)
{
    QInputDialog dlg;
    dlg.setWindowTitle(title);
    dlg.setLabelText(label);
    dlg.setOkButtonText("确定");
    dlg.setCancelButtonText("取消");
    dlg.setInputMode(QInputDialog::IntInput);
    dlg.setIntRange(minVal, maxVal);
    dlg.setIntValue(ret);
    if (dlg.exec() == QDialog::Accepted)
    {
        ret = dlg.intValue();
        return true;
    }
    return false;
}

static bool isFileExist(const char *fileName)
{
    return _access(fileName, 0) == 0;
}

class TXZMapDB
{
public:
    TXZMapDB()
    {
        load();
    }
    int getMapRecordCount() const
    {
        return (int)m_maps.size();
    }
    const TXZMapRecord* getMapRecord(int idx) const
    {
        ASSERT_INSIDE_N(idx, getMapRecordCount());
        return &m_maps[idx];
    }
    void commitMoveCnt(int idx, int moveCnt)
    {
        ASSERT_INSIDE_N(idx, getMapRecordCount());
        m_maps[idx].bestMoveCnt = min(moveCnt, m_maps[idx].bestMoveCnt);
        save();
    }
    void addNewMap(int w, int h)
    {
        verify(w * h >= MIN_MAP_WH);
        TXZMapRecord rcd;
        rcd.w = w;
        rcd.h = h;
        rcd.data.resize(w * h);
        rcd.data[0] = TT_Player;
        rcd.data[1] = TT_Box;
        rcd.data[2] = TT_Dest;
        rcd.bestMoveCnt = INIT_BEST_MOVE;
        verify(rcd.checkTXZMap());
        m_maps.push_back(rcd);
        save();
    }
    void delMap(int idx)
    {
        ASSERT_INSIDE_N(idx, getMapRecordCount());
        m_maps.erase(m_maps.begin() + idx);
        save();
    }
    void updateMap(int idx, const TXZMap* m)
    {
        ASSERT_INSIDE_N(idx, getMapRecordCount());
        verify(m->checkTXZMap());
        (TXZMap&)m_maps[idx] = *m;
        save();
    }

private:
    void load()
    {
        if (!isFileExist(TXZMAPDB_FILENAME))
        {
            std::ofstream(TXZMAPDB_FILENAME);
        }

        {
            std::ifstream fi(TXZMAPDB_FILENAME);
            verify(fi);

            std::string record;
            for (std::string line; getline(fi, line); )
            {
                if (line.empty())
                {
                    TXZMapRecord m;
                    std::istringstream si(record);
                    si >> m;
                    if (si.eof())
                    {
                        m_maps.push_back(m);
                    }
                    record.clear();
                }
                else
                {
                    record += line + "\n";
                }
            }
            if (!record.empty())
            {
                TXZMapRecord m;
                std::istringstream si(record);
                si >> m;
                if (si.eof())
                {
                    m_maps.push_back(m);
                }
            }
        }
    }
    void save()
    {
        std::ofstream fo(TXZMAPDB_FILENAME);
        for (size_t i = 0; i < m_maps.size(); ++i)
        {
            fo << m_maps[i] << "\n";
        }
    }

private:
    std::vector<TXZMapRecord> m_maps;
};

extern std::string generateSolution(const TXZMap& m, int moveLimit);

class TXZSolutionGenerateThread:
    public QThread
{
private:
    Q_OBJECT
public:
    TXZSolutionGenerateThread(
        QObject *parent, const TXZMap& map, int moveLimit):
    m_map(map), m_moveLimit(moveLimit)
    {}

protected:
    virtual void run()
    {
        emit solutionGenerated(generateSolution(m_map, m_moveLimit).c_str());
    }

signals:
    void solutionGenerated(QString s);

private:
    TXZMap m_map;
    int m_moveLimit;
};

class TXZSolutionGenerator:
    public QObject
{
private:
    Q_OBJECT

public:
    TXZSolutionGenerator(const TXZMap& m, int moveLimit):
    m_progressDlg(NULL), m_td(NULL), m_maxProgress(0)
    {
        m_maxProgress = m.getTileCnt(TT_Box) * 3;

        m_td = new TXZSolutionGenerateThread(this, m, moveLimit);
        this->connect(m_td, SIGNAL(solutionGenerated(QString)), SLOT(onSolutionGenerated(QString)));
        m_td->start();
    }
    ~TXZSolutionGenerator()
    {
        verify(m_td != NULL);
        if (m_td->isRunning())
        {
            m_td->terminate();
        }
        delete m_td; m_td = NULL;
    }

    std::string exec()
    {
        verify(m_progressDlg == NULL);
        m_progressDlg = new QProgressDialog(
            "正在自动完成关卡", "取消", 0, m_maxProgress + 1, NULL);
        m_progressDlg->setValue(0);

        QTimer t(this);
        t.setInterval(1000);
        this->connect(&t, SIGNAL(timeout()), SLOT(onTimeout()));

        t.start();
        m_progressDlg->exec();
        t.stop();

        delete m_progressDlg; m_progressDlg = NULL;

        return (const char*)m_result.toLocal8Bit();
    }

private slots:
    void onTimeout()
    {
        if (m_progressDlg != NULL)
        {
            if (m_progressDlg->value() + 1 < m_maxProgress)
            {
                m_progressDlg->setValue(m_progressDlg->value() + 1);
            }
        }
    }
    void onSolutionGenerated(QString s)
    {
        m_result = s;
        if (m_progressDlg != NULL) m_progressDlg->close();
    }

private:
    TXZSolutionGenerateThread *m_td;
    QProgressDialog *m_progressDlg;
    int          m_maxProgress;
    QString      m_result;
};

class TXZLogic:
    public QObject
{
private:
    Q_OBJECT
public:
    TXZLogic(const TXZMap& m):
    m_mp(m)
    {
        verify(m_mp.checkTXZMap());
        m_mp.getPlayerPos(m_playerX, m_playerY);
    }

    const TXZMap* getTXZMap() const
    {
        return &m_mp;
    }
    void postConstruct()
    {
        emit moveCntChanged();
    }

    int getMoveCnt() const
    {
        return (int)m_playerTrace.size();
    }
    bool move(int xOff, int yOff)
    {
        verify(qAbs(xOff) + qAbs(yOff) == 1);

        int newX = m_playerX + xOff;
        int newY = m_playerY + yOff;
        if (newX < 0 || newX >= m_mp.w || newY < 0 || newY >= m_mp.h) return false;

        TileType newTT = m_mp.getTileType(newX, newY);
        // 只有一个玩家
        verify((newTT & TT_Player) == 0);
        // 撞墙
        if ((newTT & TT_Wall) != 0) return false;
        // 有箱子
        if ((newTT & TT_Box) != 0)
        {
            int boxNewX = m_playerX + xOff * 2;
            int boxNewY = m_playerY + yOff * 2;
            if (boxNewX < 0 || boxNewX >= m_mp.w || boxNewY < 0 || boxNewY >= m_mp.h) return false;
            TileType boxNewTT = m_mp.getTileType(boxNewX, boxNewY);
            if (boxNewTT != 0 && boxNewTT != TT_Dest) return false;
            
            // 推动箱子
            m_mp.removeTileType(newX, newY, TT_Box);
            m_mp.addTileType(boxNewX, boxNewY, TT_Box);
            m_boxMoveTrace.push_back(true);
        }
        else
        {
            m_boxMoveTrace.push_back(false);
        }

        // 移动玩家
        m_mp.removeTileType(m_playerX, m_playerY, TT_Player);
        m_mp.addTileType(newX, newY, TT_Player);

        m_playerX = newX, m_playerY = newY;
        m_playerTrace.push_back(QPoint(xOff, yOff));
        emit moveCntChanged();
        return true;
    }
    void rollback(int destMoveCnt)
    {
        verify(destMoveCnt >= 0 && destMoveCnt <= getMoveCnt());
        int rollbackCnt = getMoveCnt() - destMoveCnt;
        while (rollbackCnt-- > 0)
        {
            int xOff = -m_playerTrace.back().x();
            int yOff = -m_playerTrace.back().y();
            int newX = m_playerX + xOff;
            int newY = m_playerY + yOff;

            m_mp.removeTileType(m_playerX, m_playerY, TT_Player);
            m_mp.addTileType(newX, newY, TT_Player);

            if (m_boxMoveTrace.back())
            {
                int boxX = newX + -xOff * 2;
                int boxY = newY + -yOff * 2;
                verify((m_mp.getTileType(boxX, boxY) & TT_Box) != 0);
                m_mp.removeTileType(boxX, boxY, TT_Box);
                m_mp.addTileType(m_playerX, m_playerY, TT_Box);
            }

            m_playerX = newX;
            m_playerY = newY;
            m_playerTrace.pop_back();
            m_boxMoveTrace.pop_back();
            emit moveCntChanged();
        }
    }
    bool isSuccess() const
    {
        return m_mp.isAllBoxDestOverlapping();
    }

    const std::vector<QPoint>& getPlayerTrace() const
    {
        return m_playerTrace;
    }

signals:
    void moveCntChanged();

private:
    TXZMap              m_mp;
    int                 m_playerX, m_playerY;
    std::vector<QPoint> m_playerTrace;
    std::vector<bool>   m_boxMoveTrace;
};

const int BODER_PIXEL_SIZE = 2;
class MapDisplayPanel:
    public QLabel
{
private:
    Q_OBJECT
public:
    MapDisplayPanel(): 
    m_map(NULL)
    {
        setFixedSize(400, 400);
        setMouseTracking(true);

        loadTilePixmaps();
    }

    void setTXZMap(const TXZMap* map)
    {
        m_map = map;

        if (m_map != NULL)
        {
            m_tilePixelSize = 
                min(
                32,
                min(
                (this->width() - 2 * BODER_PIXEL_SIZE) / m_map->w,
                (this->height() - 2 * BODER_PIXEL_SIZE) / m_map->h));
        }

        update();
    }

protected:
    virtual void paintEvent(QPaintEvent *evt)
    {
        QLabel::paintEvent(evt);

        QPainter painter(this);

        if (m_map == NULL)
        {
            QString s = "没有关卡";
            painter.drawText(
                size().width() / 2 - painter.fontMetrics().width(s, s.size()) / 2, 
                size().height() / 2 + painter.fontMetrics().height() / 2, s);
        }
        else
        {
            QPen boxPen(QColor(167,80,1));
            QPen destBoxPen(boxPen);
            destBoxPen.setWidth(2);
            QPen rawPen = painter.pen();

            // 世界范围
            painter.drawRect(
                0, 0, 
                m_map->w * m_tilePixelSize + BODER_PIXEL_SIZE * 2, m_map->h * m_tilePixelSize + BODER_PIXEL_SIZE * 2);

            for (int y = 0; y < m_map->h; ++y)
            {
                for (int x = 0; x < m_map->w; ++x)
                {
                    TileType tt = m_map->getTileType(x, y);

                    QPixmap pm = getTilePixmap(tt);                    
                    if (!pm.isNull())
                    {
                        painter.drawPixmap(
                            BODER_PIXEL_SIZE + x * m_tilePixelSize, 
                            BODER_PIXEL_SIZE + y * m_tilePixelSize,
                            m_tilePixelSize, m_tilePixelSize,
                            pm);
                    }
                    else
                    {
                        if ((tt & TT_Player) != 0)
                        {
                            painter.drawEllipse(
                                BODER_PIXEL_SIZE + x * m_tilePixelSize + BODER_PIXEL_SIZE,
                                BODER_PIXEL_SIZE + y * m_tilePixelSize + BODER_PIXEL_SIZE,
                                m_tilePixelSize - 2 * BODER_PIXEL_SIZE,
                                m_tilePixelSize - 2 * BODER_PIXEL_SIZE);
                        }
                        else if ((tt & TT_Box) != 0)
                        {
                            painter.setPen(boxPen);
                            painter.drawRect(
                                BODER_PIXEL_SIZE + x * m_tilePixelSize + BODER_PIXEL_SIZE,
                                BODER_PIXEL_SIZE + y * m_tilePixelSize + BODER_PIXEL_SIZE,
                                m_tilePixelSize - BODER_PIXEL_SIZE * 2,
                                m_tilePixelSize - BODER_PIXEL_SIZE * 2);

                            if ((tt & TT_Dest) != 0) painter.setPen(destBoxPen);
                            painter.drawLine(
                                BODER_PIXEL_SIZE + x * m_tilePixelSize + BODER_PIXEL_SIZE,
                                BODER_PIXEL_SIZE + y * m_tilePixelSize + BODER_PIXEL_SIZE,
                                BODER_PIXEL_SIZE + (x + 1) * m_tilePixelSize - BODER_PIXEL_SIZE,
                                BODER_PIXEL_SIZE + (y + 1) * m_tilePixelSize - BODER_PIXEL_SIZE);
                            painter.drawLine(
                                BODER_PIXEL_SIZE + x * m_tilePixelSize + BODER_PIXEL_SIZE,
                                BODER_PIXEL_SIZE + (y + 1) * m_tilePixelSize - BODER_PIXEL_SIZE,
                                BODER_PIXEL_SIZE + (x + 1) * m_tilePixelSize - BODER_PIXEL_SIZE,
                                BODER_PIXEL_SIZE + y * m_tilePixelSize + BODER_PIXEL_SIZE);

                            painter.setPen(rawPen);
                        }
                        else if ((tt & TT_Wall) != 0)
                        {
                            painter.fillRect(
                                BODER_PIXEL_SIZE + x * m_tilePixelSize,
                                BODER_PIXEL_SIZE + y * m_tilePixelSize,
                                m_tilePixelSize,
                                m_tilePixelSize, Qt::SolidPattern);
                        }
                        else if ((tt  & TT_Dest) != 0)
                        {
                            painter.drawLine(
                                BODER_PIXEL_SIZE + x * m_tilePixelSize + m_tilePixelSize / 2 - BODER_PIXEL_SIZE * 2,
                                BODER_PIXEL_SIZE + y * m_tilePixelSize + m_tilePixelSize / 2 - BODER_PIXEL_SIZE * 2,
                                BODER_PIXEL_SIZE + x * m_tilePixelSize + m_tilePixelSize / 2 + BODER_PIXEL_SIZE * 2,
                                BODER_PIXEL_SIZE + y * m_tilePixelSize + m_tilePixelSize / 2 + BODER_PIXEL_SIZE * 2);
                            painter.drawLine(
                                BODER_PIXEL_SIZE + x * m_tilePixelSize + m_tilePixelSize / 2 - BODER_PIXEL_SIZE * 2,
                                BODER_PIXEL_SIZE + y * m_tilePixelSize + m_tilePixelSize / 2 + BODER_PIXEL_SIZE * 2,
                                BODER_PIXEL_SIZE + x * m_tilePixelSize + m_tilePixelSize / 2 + BODER_PIXEL_SIZE * 2,
                                BODER_PIXEL_SIZE + y * m_tilePixelSize + m_tilePixelSize / 2 - BODER_PIXEL_SIZE * 2);
                        }
                        else{}
                    }
                }
            }

            if (m_mouseTracingTile != QPoint(-1, -1))
            {
                painter.setPen(QPen(QColor(255, 0, 0)));
                painter.drawRect(
                    BODER_PIXEL_SIZE + m_mouseTracingTile.x() * m_tilePixelSize,
                    BODER_PIXEL_SIZE + m_mouseTracingTile.y() * m_tilePixelSize,
                    m_tilePixelSize, m_tilePixelSize);
                painter.setPen(rawPen);
            }
        }
    }

    virtual void mousePressEvent(QMouseEvent *evt)
    {
        QLabel::mousePressEvent(evt);
        if (m_map == NULL) return;

        if (evt->button() == Qt::LeftButton)
        {
            this->grabMouse();

            m_oldMouseTile = QPoint(-1, -1);
            mouseClicked(QPoint(evt->x(), evt->y()));
        }
    }

    virtual void mouseReleaseEvent(QMouseEvent *evt)
    {
        QLabel::mouseReleaseEvent(evt);
        if (m_map == NULL) return;

        if (evt->button() == Qt::LeftButton)
        {
            this->releaseMouse();
        }
    }

    virtual void mouseMoveEvent(QMouseEvent *evt)
    {
        QLabel::mouseMoveEvent(evt);
        if (m_map == NULL) return;

        if (this->mouseGrabber() == this)
        {
            mouseClicked(QPoint(evt->x(), evt->y()));
        }
        
        {
            QPoint pt;
            if (mousePoint2TilePoint(QPoint(evt->x(), evt->y()), pt))
            {
                setMouseTracingTile(pt);
            }
            else setMouseTracingTile(QPoint(-1, -1));
        }
    }

signals:
    void tileClicked(int x, int y);

private:
    void mouseClicked(QPoint pt)
    {
        QPoint newMouseTile;
        if (!mousePoint2TilePoint(pt, newMouseTile)) return;
        if (m_oldMouseTile != newMouseTile)
        {
            m_oldMouseTile = newMouseTile;
            emit tileClicked(m_oldMouseTile.x(), m_oldMouseTile.y());
        }
    }
    bool mousePoint2TilePoint(QPoint mousePt, QPoint &tilePoint) const
    {
        QPoint pt(
            (mousePt.x() - BODER_PIXEL_SIZE) / m_tilePixelSize,
            (mousePt.y() - BODER_PIXEL_SIZE) / m_tilePixelSize);
        if (pt.x() < 0 || pt.x() >= m_map->w || 
            pt.y() < 0 || pt.y() >= m_map->h) return false;
        tilePoint = pt;
        return true;
    }
    void setMouseTracingTile(QPoint tracingTile)
    {
        QPoint oldTracingTile = m_mouseTracingTile;
        m_mouseTracingTile = tracingTile;
        if (oldTracingTile != m_mouseTracingTile) update();
    }

    void loadTilePixmaps()
    {
        int tab[] = 
        {
            0, TT_Dest, TT_Wall, TT_Box, TT_Player, TT_Dest | TT_Box,
        };
        const char *names[] = 
        {
            "平地", "目标", "墙", "箱子", "玩家", "目标箱子",
        };
        for (int i = 0; i < _countof(tab); ++i)
        {
            QPixmap pm(QString("images/%0.jpg").arg(names[i]));
            if (!pm.isNull()) m_pixmapDic[tab[i]] = pm;
        }
    }
    QPixmap getTilePixmap(TileType tt)
    {
        if (tt == 0);
        else if (tt == TT_Dest);
        else if (tt == TT_Wall);
        else if ((tt & TT_Player) != 0) tt = TT_Player;
        else if (tt == (TT_Box | 0)) tt = TT_Box;
        else if (tt == (TT_Box | TT_Dest));
        else verify(0);

        QMap<int, QPixmap>::const_iterator iter = m_pixmapDic.find(tt);
        if (iter == m_pixmapDic.end()) return QPixmap();
        return iter.value();
    }

private:
    const TXZMap *m_map;
    int           m_tilePixelSize;
    QPoint        m_oldMouseTile;
    QPoint        m_mouseTracingTile;
    QMap<int, QPixmap> m_pixmapDic;
};

class GameDlg:
    public QDialog
{
private:
    Q_OBJECT
public:
    GameDlg():
    m_scrollBar(Qt::Horizontal),
    m_curMapIdx(0)
    {
        this->setWindowTitle("推箱子    F1-帮助");

        m_editCurMapIdx.setReadOnly(true);
        m_editCurMapIdx.setFixedWidth(60);

        m_editBestMoveCnt.setReadOnly(true);
        m_editBestMoveCnt.setFixedWidth(50);

        m_editMoveCnt.setReadOnly(true);
        m_editMoveCnt.setFixedWidth(50);

        this->connect(&m_scrollBar, SIGNAL(valueChanged(int)), SLOT(onScrollBarValueChanged(int)));

        {
            QVBoxLayout *mainLayout = new QVBoxLayout(this);

            {
                QGroupBox *box = new QGroupBox("状态");

                QHBoxLayout *topLayout = new QHBoxLayout();
                topLayout->addWidget(new QLabel("关卡"));
                topLayout->addWidget(&m_editCurMapIdx);
                topLayout->addWidget(new QLabel("最少步数"));
                topLayout->addWidget(&m_editBestMoveCnt);
                topLayout->addWidget(new QLabel("移动步数"));
                topLayout->addWidget(&m_editMoveCnt, 0);

                box->setLayout(topLayout);
                mainLayout->addWidget(box);
            }

            mainLayout->addWidget(&m_displayPanel);

            {
                QGroupBox *box = new QGroupBox("游戏控制");

                QHBoxLayout *subLayout = new QHBoxLayout();
                subLayout->addWidget(&m_scrollBar);
                box->setLayout(subLayout);

                mainLayout->addWidget(box);
            }
        }

        if (m_mapDB.getMapRecordCount() > 0)
        {
            setCurMapIdx(0);
        }

        if (m_mapDB.getMapRecordCount() == 0)
        {
            ::MessageBox(this->winId(), "请先切换到编辑模式添加关卡", "警告", MB_OK | MB_ICONWARNING);
        }
    }

protected:
    virtual void keyReleaseEvent(QKeyEvent *evt)
    {
        QDialog::keyReleaseEvent(evt);

        switch (evt->key())
        {
        case Qt::Key_Up:
        case Qt::Key_Down:
        case Qt::Key_Left:
        case Qt::Key_Right:
            {
                if (m_logic->isSuccess()) return;
                if (evt->key() == Qt::Key_Up)
                {
                    m_logic->move(0, -1);
                }
                else if (evt->key() == Qt::Key_Down)
                {
                    m_logic->move(0, 1);
                }
                else if (evt->key() == Qt::Key_Left)
                {
                    m_logic->move(-1, 0);
                }
                else if (evt->key() == Qt::Key_Right)
                {
                    m_logic->move(1, 0);
                }
                else{}
            }
            break;

        case Qt::Key_PageDown:
        case Qt::Key_PageUp:
            {
                if (evt->key() == Qt::Key_PageDown)
                {
                    setCurMapIdx((m_curMapIdx + 1) % m_mapDB.getMapRecordCount());
                }
                else if (evt->key() == Qt::Key_PageUp)
                {
                    setCurMapIdx((m_curMapIdx - 1 + m_mapDB.getMapRecordCount()) % m_mapDB.getMapRecordCount());
                }
                else{}
            }
            break;

        case Qt::Key_Backspace:
            {
                int moveCnt = m_logic->getMoveCnt();
                if (moveCnt > 0) m_logic->rollback(moveCnt - 1);
            }
            break;

        case Qt::Key_A:
            {
                if (::MessageBox(this->winId(), "要自动完成关卡并保存录像吗？", "确认", MB_YESNO | MB_ICONQUESTION)
                    == IDYES)
                {
                    ::CreateDirectory("replay", NULL);
                    QString filePath = 
                        QFileDialog::getSaveFileName(
                        this, "保存录像", "replay", "推箱子录像(*.txt)");
                    if (filePath.isEmpty()) break;

                    TXZMap m((const TXZMap&)*m_mapDB.getMapRecord(m_curMapIdx));

                    std::string result;
                    DWORD elapsTick = 0;
                    {
                        int moveLimit = 180;
                        if (
                        !execInputDialog_Int(
                            "输入", "最大步数", 1, 99999, moveLimit)) break;

                        DWORD beginTick = ::GetTickCount();
    
                        TXZSolutionGenerator generator(m, moveLimit);
                        result = generator.exec();

                        elapsTick = ::GetTickCount() - beginTick;
                    }
                    if (result.empty())
                    {
                        ::MessageBox(NULL, "自动完成失败，可能是关卡无解或者最大移动次数太小", "错误", MB_OK | MB_ICONWARNING);
                    }
                    else
                    {
                        ::MessageBox(NULL, 
                            (const char *)QString("保存关卡成功！\n耗时:%0秒").arg(elapsTick / 1000.0f).toLocal8Bit(), 
                            "完成", MB_OK | MB_ICONINFORMATION);

                        std::ostringstream so;
                        so << "=" << result << "\n" << m;

                        TXZReplay replay;
                        std::istringstream(so.str()) >> replay;
                        std::ofstream((const char*)filePath.toLocal8Bit()) << replay;
                    }
                }
            }
            break;

        case Qt::Key_F1:
            {
                ::MessageBox(this->winId(), 
                    "F2                 ：游戏模式。\n"
                    "F3                 ：编辑模式。\n"
                    "F4                 ：回放模式。\n"
                    "\n"
                    "↑↓←→           ：移动。\n"
                    "PageDown/PageUp    ：切换关卡。\n"
                    "Backspace/滚动条   ：悔步。\n"
                    "A                  ：自动完成关卡。", 
                    "帮助 by Scan", 
                    MB_OK | MB_ICONINFORMATION);
            }
            break;

        case Qt::Key_F2:
            break;
        case Qt::Key_F3:
        case Qt::Key_F4:
            {
                createNewApp(evt->key());
                this->close();
            }
            break;

        default:
            break;
        }
    }

private:
    void setCurMapIdx(int mapIdx)
    {
        ASSERT_INSIDE_N(mapIdx, m_mapDB.getMapRecordCount());
        m_curMapIdx = mapIdx;
        m_logic.reset(new TXZLogic(*m_mapDB.getMapRecord(m_curMapIdx)));
        this->connect(m_logic.get(), SIGNAL(moveCntChanged()), SLOT(onMoveCntChanged()));

        m_displayPanel.setTXZMap(m_logic->getTXZMap());

        m_editCurMapIdx.setText(QString("%0/%1").arg(m_curMapIdx + 1).arg(m_mapDB.getMapRecordCount()));
        m_editBestMoveCnt.setText(QString("%0").arg(m_mapDB.getMapRecord(m_curMapIdx)->bestMoveCnt));

        m_logic->postConstruct();
    }

private slots:
    void onMoveCntChanged()
    {
        m_displayPanel.update();

        m_scrollBar.setRange(0, m_logic->getMoveCnt());
        m_scrollBar.setValue(m_scrollBar.maximum());

        m_editMoveCnt.setText(QString("%0").arg(m_logic->getMoveCnt()));

        if (m_logic->isSuccess())
        {
            ::MessageBox(this->winId(), "你取得了胜利", "游戏结束", MB_OK | MB_ICONINFORMATION);
            m_mapDB.commitMoveCnt(m_curMapIdx, m_logic->getMoveCnt());

            if (::MessageBox(this->winId(), "要保存录像吗？", "确认", MB_YESNO | MB_ICONQUESTION) == IDYES)
            {
                ::CreateDirectory("replay", NULL);
                QString filePath = 
                    QFileDialog::getSaveFileName(
                    this, "保存录像", "replay", "推箱子录像(*.txt)");
                if (filePath.isEmpty()) return;
                TXZReplay replay;
                replay.map = (const TXZMap&)*m_mapDB.getMapRecord(m_curMapIdx);
                replay.playerTrace = m_logic->getPlayerTrace();
                std::ofstream((const char*)filePath.toLocal8Bit()) << replay;
            }
        }
    }

    void onScrollBarValueChanged(int val)
    {
        if (m_scrollBar.value() == m_scrollBar.maximum()) return;
        m_logic->rollback(m_scrollBar.value());
    }

private:
    std::auto_ptr<TXZLogic> m_logic;
    TXZMapDB                m_mapDB;
    int                     m_curMapIdx;
    QLineEdit               m_editCurMapIdx;
    QLineEdit               m_editBestMoveCnt;
    MapDisplayPanel         m_displayPanel;
    QScrollBar              m_scrollBar;
    QLineEdit               m_editMoveCnt;
};

class EditerDlg:
    public QDialog
{
private:
    Q_OBJECT
public:
    EditerDlg():
    m_editMapIdx(0),
    m_isEditing(false)
    {
        setWindowTitle("推箱子编辑器    F1-帮助");

        QVBoxLayout *mainLayout = new QVBoxLayout(this);

        {
            m_editMapIdx.setFixedWidth(60);
            m_editMapIdx.setReadOnly(true);

            QHBoxLayout *topLaytout = new QHBoxLayout();
            topLaytout->addWidget(new QLabel("关卡"));
            topLaytout->addWidget(&m_editMapIdx);

            {
                QGroupBox *box = new QGroupBox("关卡操作");

                m_btnAddMap.setText("添加");
                m_btnDelMap.setText("删除");
                m_btnEditMap.setText("编辑");

                this->connect(&m_btnAddMap, SIGNAL(clicked(bool)), SLOT(onButtonClicked(bool)));
                this->connect(&m_btnDelMap, SIGNAL(clicked(bool)), SLOT(onButtonClicked(bool)));
                this->connect(&m_btnEditMap, SIGNAL(clicked(bool)), SLOT(onButtonClicked(bool)));

                QHBoxLayout *subLayout = new QHBoxLayout();
                subLayout->addWidget(&m_btnAddMap);
                subLayout->addWidget(&m_btnDelMap);
                subLayout->addWidget(&m_btnEditMap);
                box->setLayout(subLayout);

                topLaytout->addWidget(box);
            }

            mainLayout->addLayout(topLaytout);
        }

        {
            m_radioBtnPlayer.setText("玩家");
            m_radioBtnBox.setText("箱子");
            m_radioBtnWall.setText("墙");
            m_radioBtnDest.setText("目标");
            m_radioBtnNull.setText("平地");

            this->connect(&m_radioBtnPlayer, SIGNAL(clicked(bool)), SLOT(onButtonClicked(bool)));
            this->connect(&m_radioBtnBox, SIGNAL(clicked(bool)), SLOT(onButtonClicked(bool)));
            this->connect(&m_radioBtnWall, SIGNAL(clicked(bool)), SLOT(onButtonClicked(bool)));
            this->connect(&m_radioBtnDest, SIGNAL(clicked(bool)), SLOT(onButtonClicked(bool)));
            this->connect(&m_radioBtnNull, SIGNAL(clicked(bool)), SLOT(onButtonClicked(bool)));

            QGroupBox *box = new QGroupBox("笔刷");
            QHBoxLayout *subLayout = new QHBoxLayout();
            subLayout->addWidget(&m_radioBtnWall);
            subLayout->addWidget(&m_radioBtnDest);
            subLayout->addWidget(&m_radioBtnNull);
            subLayout->addWidget(&m_radioBtnBox);
            subLayout->addWidget(&m_radioBtnPlayer);
            box->setLayout(subLayout);

            mainLayout->addWidget(box);
        }

        {
            this->connect(&m_displayPanel, SIGNAL(tileClicked(int,int)), SLOT(onTileClicked(int,int)));
            mainLayout->addWidget(&m_displayPanel);
        }

        if (m_mapDB.getMapRecordCount() > 0) setCurMapIdx(0);
        setIsEditing(false);

        if (m_mapDB.getMapRecordCount() == 0)
        {
            ::MessageBox(this->winId(), "请先添加关卡", "警告", MB_OK | MB_ICONWARNING);
        }
    }

protected:
    virtual void keyReleaseEvent(QKeyEvent *evt)
    {
        QDialog::keyReleaseEvent(evt);

        switch (evt->key())
        {
        case Qt::Key_PageDown:
        case Qt::Key_PageUp:
            {
                if (m_isEditing)
                {
                    if (::MessageBox(this->winId(), "要结束编辑状态吗？", "确认", MB_YESNO | MB_ICONQUESTION) == IDNO) return;
                    if (!setIsEditing(false)) return;
                }

                if (evt->key() == Qt::Key_PageDown)
                {
                    setCurMapIdx((m_curMapIdx + 1) % m_mapDB.getMapRecordCount());
                }
                else if (evt->key() == Qt::Key_PageUp)
                {
                    setCurMapIdx((m_curMapIdx - 1 + m_mapDB.getMapRecordCount()) % m_mapDB.getMapRecordCount());
                }
                else {}
            }
            break;

        case Qt::Key_F1:
            {
                ::MessageBox(this->winId(), 
                    "F2                 ：游戏模式。\n"
                    "F3                 ：编辑模式。\n"
                    "F4                 ：回放模式。\n"
                    "\n",
                    "帮助 by Scan",
                    MB_OK | MB_ICONINFORMATION);
            }
            break;

        case Qt::Key_F3:
            break;
        case Qt::Key_F2:
        case Qt::Key_F4:
            {
                createNewApp(evt->key());
                this->close();
            }
            break;

        default:
            break;
        }
    }

private:
    void setCurMapIdx(int mapIdx)
    {
        verify(!m_isEditing);
        ASSERT_INSIDE_N(mapIdx, m_mapDB.getMapRecordCount());
        m_curMapIdx = mapIdx;
        m_editingMap = *m_mapDB.getMapRecord(m_curMapIdx);

        m_editMapIdx.setText(QString("%0/%1").arg(m_curMapIdx + 1).arg(m_mapDB.getMapRecordCount()));

        m_displayPanel.setTXZMap(&m_editingMap);
    }
    bool setIsEditing(bool isEditing)
    {
        if (m_isEditing && !isEditing)
        {
            if (!m_editingMap.checkTXZMap())
            {
                ::MessageBox(this->winId(), "当前关卡无效！", "错误", MB_OK | MB_ICONWARNING);
                return false;
            }
            m_mapDB.updateMap(m_curMapIdx, &m_editingMap);
        }

        m_isEditing = isEditing;

        m_btnAddMap.setEnabled(!m_isEditing);
        m_btnDelMap.setEnabled(!m_isEditing);
        m_btnEditMap.setText(m_isEditing ? "完成" : "编辑");
        m_radioBtnNull.setEnabled(m_isEditing);
        m_radioBtnDest.setEnabled(m_isEditing);
        m_radioBtnPlayer.setEnabled(m_isEditing);
        m_radioBtnWall.setEnabled(m_isEditing);
        m_radioBtnBox.setEnabled(m_isEditing);

         if (m_isEditing) m_radioBtnPlayer.setChecked(true);

        return true;
    }

private slots:
    void onButtonClicked(bool checked)
    {
        if (QPushButton* btn = qobject_cast<QPushButton*>(this->focusWidget()))
        {
            if (btn == &m_btnAddMap)
            {
                int w = MIN_MAP_WH;
                int h = 1;
                if (!execInputDialog_Int("输入", "输入宽度", 1, MAX_MAP_SIZE, w)) return;
                if (w < MIN_MAP_WH) h = qCeil(float(MIN_MAP_WH) / w) * w;
                if (!execInputDialog_Int("输入", "输入高度", h, MAX_MAP_SIZE, h)) return;
                m_mapDB.addNewMap(w, h);
                setCurMapIdx(m_mapDB.getMapRecordCount() - 1);
            }
            else if (btn == &m_btnDelMap)
            {
                if (m_mapDB.getMapRecordCount() < 2)
                {
                    ::MessageBox(winId(), "当前关卡太少，不能删除！", "失败", MB_OK | MB_ICONINFORMATION);
                    return;
                }
                if (::MessageBox(winId(), "确定要删除该图吗？", "确认", MB_YESNO | MB_ICONQUESTION) == IDYES)
                {
                    m_mapDB.delMap(m_curMapIdx);
                    setCurMapIdx(m_curMapIdx % m_mapDB.getMapRecordCount());
                }
            }
            else if (btn == &m_btnEditMap)
            {
                setIsEditing(!m_isEditing);
            }
            else{}
        }
        if (QRadioButton* btn = qobject_cast<QRadioButton*>(this->focusWidget()))
        {
        }
    }

    void onTileClicked(int x, int y)
    {
        if (!m_isEditing) return;
        ASSERT_INSIDE_N(x, m_editingMap.w);
        ASSERT_INSIDE_N(y, m_editingMap.h);

        if (m_radioBtnNull.isChecked())
        {
            m_editingMap.setTileType(x, y, (TileType)0);
        }
        else if (m_radioBtnDest.isChecked())
        {
            m_editingMap.setTileType(x, y, TT_Dest);
        }
        else if (m_radioBtnWall.isChecked())
        {
            m_editingMap.setTileType(x, y, TT_Wall);
        }
        else if (m_radioBtnBox.isChecked())
        {
            TileType tt = m_editingMap.getTileType(x, y);
            if ((tt & TT_Wall) != 0) return;
            if ((tt & TT_Player) != 0) 
            {
                m_editingMap.removeTileType(x, y, TT_Player);
                tt = m_editingMap.getTileType(x, y);
            }
            if ((tt & TT_Box) != 0) 
            {
                m_editingMap.removeTileType(x, y, TT_Box);
            }
            else
            {
                verify(tt == 0 || tt == TT_Dest);
                m_editingMap.addTileType(x, y, TT_Box);
            }
        }
        else if (m_radioBtnPlayer.isChecked())
        {
            TileType tt = m_editingMap.getTileType(x, y);
            if ((tt & TT_Wall) != 0) return;
            if ((tt & TT_Box) != 0) 
            {
                m_editingMap.removeTileType(x, y, TT_Box);
                tt = m_editingMap.getTileType(x, y);
            }
            if ((tt & TT_Player) != 0) 
            {
                m_editingMap.removeTileType(x, y, TT_Player);
            }
            else
            {
                verify(tt == 0 || tt == TT_Dest);
                m_editingMap.addTileType(x, y, TT_Player);
            }
        }
        else{}

        m_displayPanel.update();
    }

private:
    TXZMapDB    m_mapDB;
    TXZMap      m_editingMap;

    int         m_curMapIdx;
    bool        m_isEditing;
    QLineEdit   m_editMapIdx;
    QPushButton m_btnAddMap;
    QPushButton m_btnDelMap;
    QPushButton m_btnEditMap;

    QRadioButton m_radioBtnNull, m_radioBtnDest, m_radioBtnPlayer, m_radioBtnWall, m_radioBtnBox;
    MapDisplayPanel m_displayPanel;
};

class ReplayDlg:
    public QDialog
{
private:
    Q_OBJECT
public:
    ReplayDlg():
    m_scrollReplayProgressCtrl(Qt::Horizontal)
    {
        setWindowTitle("推箱子录像回放机    F1-帮助");
        setMouseTracking(true);

        QVBoxLayout *mainLayout = new QVBoxLayout(this);

        {
            m_btnLoad.setText("加载");
            m_editCurReplay.setReadOnly(true);

            this->connect(&m_btnLoad, SIGNAL(clicked(bool)), SLOT(onLoadButtonClicked(bool)));

            QGroupBox *box = new QGroupBox("录像选择");
            QHBoxLayout *subLayout = new QHBoxLayout();
            subLayout->addWidget(&m_btnLoad);
            subLayout->addWidget(new QLabel("当前录像"));
            subLayout->addWidget(&m_editCurReplay);
            box->setLayout(subLayout);

            mainLayout->addWidget(box);
        }

        {
            m_btnLoad.setText("加载");
            m_editReplayProgress.setReadOnly(true);
            m_editReplayProgress.setFixedWidth(80);

            this->connect(&m_scrollReplayProgressCtrl, SIGNAL(valueChanged(int)), SLOT(onScrollBarValueChanged(int)));

            QGroupBox *box = new QGroupBox("播放控制");
            QHBoxLayout *subLayout = new QHBoxLayout();
            subLayout->addWidget(new QLabel("进度"));
            subLayout->addWidget(&m_editReplayProgress);
            subLayout->addWidget(&m_scrollReplayProgressCtrl, 1);
            box->setLayout(subLayout);

            mainLayout->addWidget(box);
        }

        mainLayout->addWidget(&m_displayPanel);

        loadReplay("");
    }

protected:
    virtual void keyReleaseEvent(QKeyEvent *evt)
    {
        QDialog::keyReleaseEvent(evt);

        switch (evt->key())
        {
        case Qt::Key_Left:
        case Qt::Key_Right:
            {
                addReplayProgress(evt->key() == Qt::Key_Left ? -1 : 1);
            }
            break;

        case Qt::Key_F1:
            ::MessageBox(this->winId(), 
                "F2                 ：游戏模式。\n"
                "F3                 ：编辑模式。\n"
                "F4                 ：回放模式。\n"
                "\n"
                "鼠标滚轮/←→      ：控制进度。", 
                "帮助 by Scan", 
                MB_OK | MB_ICONINFORMATION);
            break;

        case Qt::Key_F2:
        case Qt::Key_F3:
            {
                createNewApp(evt->key());
                this->close();
            }
            break;
        case Qt::Key_F4:
            break;

        default:
            break;
        }
    }

    virtual void wheelEvent(QWheelEvent *evt)
    {
        QDialog::wheelEvent(evt);

        addReplayProgress(evt->delta() > 0 ? 1 : (evt->delta() < 0 ? -1 : 0));
    }

private slots:
    void onLoadButtonClicked(bool checked)
    {
        ::CreateDirectory("replay", NULL);
        QString fileName = 
        QFileDialog::getOpenFileName(
            this, "打开录像", "replay", "推箱子录像(*.txt)");
        if (fileName.isEmpty()) return;
        loadReplay(fileName);
    }

    void onScrollBarValueChanged(int val)
    {
        setReplayProgress(val);
    }

private:
    void loadReplay(const QString& fileName)
    {
        if (fileName.isEmpty())
        {
            m_displayPanel.setTXZMap(NULL);
            m_logic.reset();
            m_replay.reset();

            m_editCurReplay.setText("");
            m_scrollReplayProgressCtrl.setRange(0, 0);
        }
        else
        {
            m_replay.reset(new TXZReplay());
            std::ifstream((const char *)fileName.toLocal8Bit()) >> *m_replay;

            m_logic.reset(new TXZLogic(m_replay->map));
            m_displayPanel.setTXZMap(m_logic->getTXZMap());

            m_editCurReplay.setText(fileName);
            m_scrollReplayProgressCtrl.setRange(0, (int)m_replay->playerTrace.size());
        }

        setReplayProgress(0);
    }

    void setReplayProgress(int progress)
    {
        if (m_logic.get() != NULL)
        {
            if (progress < m_logic->getMoveCnt())
            {
                m_logic->rollback(progress);
            }
            else if (progress > m_logic->getMoveCnt())
            {
                for (int i = m_logic->getMoveCnt(); i < progress; ++i)
                {
                    QPoint moveTrace = m_replay->playerTrace[i];
                    m_logic->move(moveTrace.x(), moveTrace.y());
                }
            }
            else {}
            m_displayPanel.update();
        }

        m_editReplayProgress.setText(QString("%0/%1").
            arg(progress).
            arg(m_replay.get() != NULL ? (int)m_replay->playerTrace.size() : 0));
        m_scrollReplayProgressCtrl.setValue(progress);
    }

    void addReplayProgress(int off)
    {
        int maxProgress = m_replay.get() != NULL ? (int)m_replay->playerTrace.size() : 0;
        int curProgress = m_logic.get() != NULL ? m_logic->getMoveCnt() : 0;
        setReplayProgress(qBound(0, curProgress + off, maxProgress));
    }

private:
    QPushButton             m_btnLoad;
    QLineEdit               m_editCurReplay;

    QLineEdit               m_editReplayProgress;
    QScrollBar              m_scrollReplayProgressCtrl;

    std::auto_ptr<TXZReplay> m_replay;
    std::auto_ptr<TXZLogic> m_logic;
    MapDisplayPanel         m_displayPanel;
};

int main(int argc, char *argv[])
{
    QApplication app(argc, argv);

    app.addLibraryPath(app.applicationDirPath() + "/plugin");
    QTextCodec::setCodecForCStrings(QTextCodec::codecForName("gb18030"));

    setlocale(LC_ALL, "");

    std::auto_ptr<QDialog> dlg;
    if (argc > 1 && std::string(argv[1]) == "edit") dlg.reset(new EditerDlg());
    if (argc > 1 && std::string(argv[1]) == "replay") dlg.reset(new ReplayDlg());
    if (dlg.get() == NULL) dlg.reset(new GameDlg());
    dlg->show();

    app.exec();
}

int WINAPI WinMain( __in HINSTANCE hInstance, __in_opt HINSTANCE hPrevInstance, __in_opt LPSTR lpCmdLine, __in int nShowCmd )
{
    return main(__argc, __argv);
}

#include "moc/Console.cpp.cpp"