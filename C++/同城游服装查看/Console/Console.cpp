// Console.cpp : 定义控制台应用程序的入口点。
//

#include "stdafx.h"

class DrawPanel : public QLabel
{
private:
    Q_OBJECT
public:
    DrawPanel(QString path):
      m_imgIdx(0)
    {
        m_imgList.push_back(QImage(path));
        m_imgList.push_back(QImage(QString(path).replace("_p.bmp", "_w.bmp")));

        setMaximumSize(m_imgList[0].width() / 2, m_imgList[1].height());
    }

    void nextImage()
    {
        if (m_imgList.empty()) return;
        m_imgIdx = (m_imgIdx + 1) % m_imgList.size();
        this->update();
    }

protected:
    virtual void paintEvent(QPaintEvent *e)
    {
        QPainter painter(this);
        
        QImage img = m_imgList[m_imgIdx];

        painter.setCompositionMode(QPainter::RasterOp_SourceAndDestination);
        painter.drawImage(0, 0, img, img.width() / 2, 0, img.width(), img.height());

        painter.setCompositionMode(QPainter::RasterOp_SourceOrDestination);
        painter.drawImage(0, 0, img, 0, 0, img.width(), img.height());
    }

private:
    QList<QImage> m_imgList;
    int m_imgIdx;
};

class App : QObject
{ 
private:
    Q_OBJECT

public:
    App(int argc, char *argv[]):
    m_app(argc, argv),
    m_rootDir("G:/软件/GameChannel/share/Image/Clothing/")
    {
        m_dlg = new QDialog();
        m_dlg->setWindowFlags(m_dlg->windowFlags() | Qt::WindowMaximizeButtonHint);

        QHBoxLayout* layout1 = new QHBoxLayout(m_dlg);

        QListWidget* list = new QListWidget(m_dlg);
        layout1->addWidget(list, 1);
        QLabel* panel = new QLabel(m_dlg);
        layout1->addWidget(panel, 4);

        m_layout = new QGridLayout(panel);
    
        ////////////////////////////////////////////////////////////////

        QStringList subDirs = QDir(m_rootDir).entryList(QDir::Dirs | QDir::NoDotAndDotDot);
        list->addItems(subDirs);

        this->connect(list, 
            SIGNAL(currentItemChanged(QListWidgetItem*, QListWidgetItem*)), 
            SLOT(list_currentItemChanged(QListWidgetItem*, QListWidgetItem*)));

        m_dlg->show();

        m_timer = new QTimer();
        m_timer->setInterval(500);
        m_timer->start();
        this->connect(m_timer, SIGNAL(timeout()), SLOT(timer_timeout()));
    }

    ~App()
    {
        delete m_timer;
        delete m_dlg;
    }

    int exec() { return m_app.exec(); }

private slots:
    void list_currentItemChanged(QListWidgetItem *current, QListWidgetItem *previous)
    {
        Q_FOREACH(QObject *o, m_layout->parentWidget()->children())
        {
            if (QWidget *drawPanel = qobject_cast<QWidget*>(o))
            {
                drawPanel->setParent(NULL);
                delete drawPanel;
            }
        }

        QString dir = m_rootDir + current->text();
        
        int i = 0;
        QRegExp reg("\\d+_\\d+_p.bmp");
        Q_FOREACH(QString file, QDir(dir).entryList(QDir::Files))
        {
            if (reg.indexIn(file) >= 0)
            {
                DrawPanel* panel = new DrawPanel(dir + '/' + file);
                m_layout->addWidget(panel, i / 4, i % 4);
                ++i;
            }
        }
    }

    void timer_timeout()
    {
        Q_FOREACH(QObject *o, m_layout->parentWidget()->children())
        {
            if (DrawPanel *drawPanel = qobject_cast<DrawPanel*>(o))
            {
                drawPanel->nextImage();
            }
        }
    }

private:
    QApplication m_app;
    QDialog* m_dlg;
    QString m_rootDir;
    QGridLayout *m_layout;
    QTimer *m_timer;
};

#include "moc/Console.cpp.cpp"

int main(int argc, char *argv[])
{
    QTextCodec::setCodecForCStrings(QTextCodec::codecForLocale());

    App app(argc, argv);
    return app.exec();
}