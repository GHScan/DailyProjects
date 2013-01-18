// Console.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"

#include <cassert>

#include <lua.hpp>
#pragma comment(lib, "D:/Lua/5.1/lib/lua5.1.lib")

inline double smoothStep(double minVal, double maxVal, double val)
{
    if (val < minVal) return 0;
    if (val > maxVal) return 1;
    return (val - minVal) / (maxVal - minVal);
}

inline double lerp(double minVal, double maxVal, double val01)
{
    assert(val01 >= 0 && val01 <= 1);
    return minVal + (maxVal - minVal) * val01;
}

class DrawPanel:
    public QLabel
{
private:
    Q_OBJECT

public:
    DrawPanel(QWidget* parent): 
    QLabel(parent),
    m_minX(-5), m_maxX(5), m_minY(-5), m_maxY(5)
    {
        setMinimumSize(300, 300);

        m_luaState = lua_open();
        luaL_openlibs(m_luaState);

        if (luaL_dofile(m_luaState, "init.lua") != 0)
        {
            lua_pop(m_luaState, -1);
        }
    }

    ~DrawPanel()
    {
        lua_close(m_luaState);
    }

    double getMinX() const { return m_minX; }
    double getMaxX() const { return m_maxX; }
    double getMinY() const { return m_minY; }
    double getMaxY() const { return m_maxY; }
    
signals:
    void luaErrorDetected(const QString& s);

public slots:
    void onXRangeChanged(double minX, double maxX)
    {
        m_minX = minX, m_maxX = maxX;
        resetResults();
        update();
    }

    void onYRangeChanged(double minY, double maxY)
    {
        m_minY = minY, m_maxY = maxY;
        update();
    }

    void onExpressionChanged(const QString& exp)
    {
        QString s;
        if (exp.startsWith("lua:"))
        { // 要执行lua语句
            s = exp.right(exp.size() - 4);
        }   
        else
        {
            s = "function f(x) return " + exp + " end";
        }

        if (luaL_loadstring(
                m_luaState, 
                s.toStdString().c_str()) || 
            lua_pcall(m_luaState, 0, 0, 0))
        {
            emit luaErrorDetected(lua_tostring(m_luaState, -1));
            lua_pop(m_luaState, 1);
            return;
        }

        resetResults();
        update();
    }

private:
    virtual void paintEvent(QPaintEvent *e)
    {
        if (size().width() != m_results.size()) return;
        if (size().width() <= 1 || size().height() <= 1) return;

        QSize sz = size();

        QPainter painter(this);
        // 绘制曲线
        {
            QVector<QPoint> lines;
            for (int x = 0; x < sz.width() - 1; ++x)
            {
                int y1 = (1 - smoothStep(m_minY, m_maxY, m_results[x])) * sz.height();
                int y2 = (1 - smoothStep(m_minY, m_maxY, m_results[x + 1])) * sz.height();

                lines.push_back(QPoint(x, y1));
                lines.push_back(QPoint(x + 1, y2));
            }
            painter.drawLines(lines);
        }
        // 绘制x、y轴
        {
            const int FONT_OFF = 15;
            painter.setPen(QColor(0, 0, 255));
            int y = (1 - smoothStep(m_minY, m_maxY, 0)) * sz.height();
            painter.drawLine(QPoint(0, y), QPoint(sz.width(), y));
            painter.drawText(sz.width() - FONT_OFF, sz.height() / 2 - FONT_OFF, "+x");
            int x = smoothStep(m_minX, m_maxX, 0) * sz.width();
            painter.drawLine(QPoint(x,  0), QPoint(x, sz.height()));
            painter.drawText(sz.width() / 2 + FONT_OFF, 0 + FONT_OFF, "+y");
        }
    }

    virtual void resizeEvent(QResizeEvent *)
    {
        resetResults();
    }

private:
    void resetResults()
    {
        m_results.clear();

        int w = size().width();
        m_results.resize(w);
        for (int i = 0; i < w; ++i)
        {
            double y = 0;
            if (!callLuaF(lerp(m_minX, m_maxX, i / double(w)), y)) break;
            m_results[i] = y;
        }
    }

    bool callLuaF(double x, double &y)
    {
        bool error = true;
        do
        {
            lua_getglobal(m_luaState, "f");
            if (lua_isnil(m_luaState, -1)) break;
            lua_pushnumber(m_luaState, x);
            if (lua_pcall(m_luaState, 1, 1, 0) != 0) break;
            if (lua_isnumber(m_luaState, -1) == 0) break;
            y = lua_tonumber(m_luaState, -1);
            lua_pop(m_luaState, 1);

            error = false;
        }while (0);

        if (error)
        {
            emit luaErrorDetected(lua_tostring(m_luaState, -1));
            lua_pop(m_luaState, -1);
            return false;
        }

        return true;
    }

private:
    lua_State *m_luaState;
    double m_minX, m_maxX, m_minY, m_maxY;
    QVector<double>  m_results;
};

class MainDlg:
    public QDialog
{
private:
    Q_OBJECT

public:
    MainDlg():
      m_drawPanel(this), m_editMinX(this), m_editMaxX(this), 
      m_editMinY(this), m_editMaxY(this), 
      m_editExpression(this), m_btnApplyExp(this),
      m_editExpError(this)
    {
        QVBoxLayout *l1 = new QVBoxLayout(this);
        l1->addWidget(&m_drawPanel);
        {
            QHBoxLayout *l2 = new QHBoxLayout();
            l2->addWidget(new QLabel("-x", this));
            l2->addWidget(&m_editMinX);
            l2->addWidget(new QLabel("x", this));
            l2->addWidget(&m_editMaxX);
            l1->addLayout(l2);
        }
        {
            QHBoxLayout *l2 = new QHBoxLayout();
            l2->addWidget(new QLabel("-y", this));
            l2->addWidget(&m_editMinY);
            l2->addWidget(new QLabel("y", this));
            l2->addWidget(&m_editMaxY);
            l1->addLayout(l2);
        }
        l1->addWidget(&m_editExpression);
        {
            QHBoxLayout *l2 = new QHBoxLayout();
            l2->addWidget(new QLabel("错误", this));
            l2->addWidget(&m_editExpError);
            l1->addLayout(l2);
        }
        l1->addWidget(&m_btnApplyExp);
        
        /////////////////////////////////////////////////////////////////////

        setWindowTitle("表达式绘制器");
        setWindowFlags(windowFlags() | Qt::WindowMaximizeButtonHint);

        m_editMinX.setValidator(new QDoubleValidator(this));
        m_editMaxX.setValidator(new QDoubleValidator(this));
        m_editMinY.setValidator(new QDoubleValidator(this));
        m_editMaxY.setValidator(new QDoubleValidator(this));

        m_editMinX.setText(QString::number(m_drawPanel.getMinX()));
        m_editMaxX.setText(QString::number(m_drawPanel.getMaxX()));
        m_editMinY.setText(QString::number(m_drawPanel.getMinY()));
        m_editMaxY.setText(QString::number(m_drawPanel.getMaxY()));

        m_editExpression.setSizePolicy(QSizePolicy::Preferred, QSizePolicy::Maximum);

        m_editExpError.setReadOnly(true);

        m_btnApplyExp.setText("应用表达式");

        /////////////////////////////////////////////////////////////////////

        m_drawPanel.connect(this, SIGNAL(xRangeChanged(double, double)), SLOT(onXRangeChanged(double, double)));
        m_drawPanel.connect(this, SIGNAL(yRangeChanged(double, double)), SLOT(onYRangeChanged(double, double)));
        m_drawPanel.connect(this, SIGNAL(expressionChanged(const QString&)), SLOT(onExpressionChanged(const QString&)));

        m_editExpError.connect(&m_drawPanel, SIGNAL(luaErrorDetected(const QString&)), SLOT(setText(const QString&)));

        connect(&m_editMinX, SIGNAL(textChanged(const QString &)), SLOT(onRangeEdit_textChanged()));
        connect(&m_editMaxX, SIGNAL(textChanged(const QString &)), SLOT(onRangeEdit_textChanged()));
        connect(&m_editMinY, SIGNAL(textChanged(const QString &)), SLOT(onRangeEdit_textChanged()));
        connect(&m_editMaxY, SIGNAL(textChanged(const QString &)), SLOT(onRangeEdit_textChanged()));
        connect(&m_btnApplyExp, SIGNAL(clicked()), SLOT(onApplyExpButton_Clicked()));
    }

    ~MainDlg()
    {
        
    }

private:
    virtual void keyReleaseEvent(QKeyEvent *ev)
    {
        if (ev->key() == Qt::Key_F1)
        {
            QMessageBox::about(
                this, 
                "提示", 
                "@1. 表达式框中, 编写x的表达式, 该表达式将被显示为曲线."
                "@2. 表达式框中, 以'lua:'开头, 表示直接作为lua语句执行."
                "@3. 在'init.lua'文件中可以做一些初始化工作.");
        }
    }

signals:
    void xRangeChanged(double minX, double maxX);
    void yRangeChanged(double minY, double maxY);
    void expressionChanged(const QString& exp);

public slots:
    void onRangeEdit_textChanged()
    {
        emit xRangeChanged(m_editMinX.text().toDouble(), m_editMaxX.text().toDouble());
        emit yRangeChanged(m_editMinY.text().toDouble(), m_editMaxY.text().toDouble());
    }
    void onApplyExpButton_Clicked()
    {
        emit expressionChanged(m_editExpression.document()->toPlainText());
    }

private:
    DrawPanel   m_drawPanel;
    QLineEdit   m_editMinX, m_editMaxX;
    QLineEdit   m_editMinY, m_editMaxY;
    QTextEdit   m_editExpression;
    QLineEdit   m_editExpError;
    QPushButton m_btnApplyExp;
};

#include "moc/console.cpp.cpp"

int main(int argc, char *argv[])
{
    QTextCodec::setCodecForCStrings(QTextCodec::codecForName("gb18030"));

    QApplication app(argc, argv);

    MainDlg dlg;
    dlg.show();

    return app.exec(); 
}