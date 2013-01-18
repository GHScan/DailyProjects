#include "stdafx.h"

#include <windows.h>
#undef min
#undef max

#include "DlgLineCounter.h"

class CounterThread:
    public QThread
{
private:
    Q_OBJECT

public:
    CounterThread();
    ~CounterThread();

    void stopLineCount();

public slots:
    void slot_filesQueued(const QStringList& files, QObject *worker);
    void slot_startCountLine();

signals:
    void signal_lineCountOver(const QString& file, int lineCnt);

protected:
    virtual void run();

private:
    bool    m_countLine;
};

#include "DlgLineCounter.moc"

CounterThread::CounterThread():
m_countLine(true)
{
}

CounterThread::~CounterThread()
{
}

void CounterThread::run()
{
    exec();
}

void CounterThread::slot_startCountLine()
{
    m_countLine = true;
}

void CounterThread::stopLineCount()
{
    m_countLine = false;
}

void CounterThread::slot_filesQueued(const QStringList& files, QObject *worker)
{
    if (worker != this) return;

    Q_FOREACH(const QString& file, files)
    {
        // QCoreApplication::processEvents();
        if (!m_countLine)
        {
            break;
        }

        int line = 0;

        QFile f(file);
        if (!f.open(QFile::ReadOnly | QFile::Text))
        {
            continue;
        }

        QTextStream ts(&f);
        while (!ts.atEnd())
        {
            ts.readLine();
            ++line;
        }

        emit signal_lineCountOver(file, line);
    }
}



DlgLineCounter::DlgLineCounter(QWidget *parent, Qt::WFlags flags): 
QDialog(parent, flags)
{
    m_ui.setupUi(this);

    {
        QSettings setting("ScanSoft", QCoreApplication::instance()->applicationName());
        m_ui.m_edtExt->setText(setting.value("fileExtensions").toString());
    }

    SYSTEM_INFO info = {0};
    GetSystemInfo(&info);
    for (int i = 0; i < info.dwNumberOfProcessors; ++i)
    {
        CounterThread *td = new CounterThread();
        m_threads.push_back(td);

        QObject::connect(
            m_threads.back(), SIGNAL(signal_lineCountOver(const QString&,int)), 
            this, SLOT(slot_lineCountOver(const QString&,int)),
            Qt::QueuedConnection);

        QObject::connect(
            this, SIGNAL(signal_filesQueued(const QStringList&,QObject*)), 
            m_threads.back(), SLOT(slot_filesQueued(const QStringList&,QObject*)),
            Qt::QueuedConnection);

        QObject::connect(
            this, SIGNAL(signal_startCountLine()), 
            m_threads.back(), SLOT(slot_startCountLine()),
            Qt::BlockingQueuedConnection);

        td->moveToThread(td);
        td->start();
    }

    startTimer(100);
}

DlgLineCounter::~DlgLineCounter()
{
    Q_FOREACH(QObject *obj, m_threads)
    {
        CounterThread *td = qobject_cast<CounterThread*>(obj);
        td->stopLineCount();
    }
    emit signal_startCountLine();

    Q_FOREACH(QObject *obj, m_threads)
    {
        CounterThread *td = qobject_cast<CounterThread*>(obj);
        td->quit();
        td->wait(3000);
        if (!td->isFinished())
        {
            td->terminate();
        }
        delete td;
    }

    m_threads.clear();

    {
        QSettings setting("ScanSoft", QCoreApplication::instance()->applicationName());
        setting.setValue("fileExtensions", m_ui.m_edtExt->text());
    }
}

void DlgLineCounter::dragEnterEvent(QDragEnterEvent *evt)
{
    if (evt->mimeData()->hasUrls())
    {
        evt->acceptProposedAction();
    }
}

void DlgLineCounter::dropEvent(QDropEvent *evt)
{
    if (!evt->mimeData()->hasUrls()) return;

    Q_FOREACH(QObject *obj, m_threads)
    {
        CounterThread *td = qobject_cast<CounterThread*>(obj);
        td->stopLineCount();
    }
    emit signal_startCountLine();

    m_ui.m_edtFilePath->setText(evt->mimeData()->urls().first().toLocalFile());
    m_ui.m_edtFileCount->setProperty("value", 0);
    m_ui.m_edtLineCount->setProperty("value", 0);

    QStringList extFilters = m_ui.m_edtExt->text().split(";");
    if (extFilters.isEmpty()) return;
    for (int i = 0; i < extFilters.size(); ++i)
    {
        QString ext = extFilters[i];
        int idx = ext.lastIndexOf("*.");
        if (idx != -1)
        {
            ext = ext.mid(idx + 1);
        }
        extFilters[i] = "*." + ext;
    }

    QStringList paths;
    QStringList files;
    QList<QUrl> urls = evt->mimeData()->urls();
    Q_FOREACH(const QUrl& url, urls)
    {
        QString file = url.toLocalFile();
        QFileInfo info(file);
        if (info.isFile() && extFilters.indexOf("*." + info.suffix()) != -1)
        {
            files.push_back(file);
        }
        else if (info.isDir())
        {
            paths.push_back(file);
        }
        else {}
    }

    Q_FOREACH(const QString& path, paths)
    {
        QDirIterator iter(path, extFilters, QDir::Files | QDir::Hidden | QDir::System | QDir::NoSymLinks, QDirIterator::Subdirectories);
        while (iter.hasNext())
        {
            files.push_back(iter.next());
            m_ui.m_lblIteredFile->setProperty("value", tr("扫描: ") + files.back());

            QCoreApplication::processEvents();
        }
    }   
    m_ui.m_lblIteredFile->setProperty("value", tr("扫描结束"));
    paths.clear();
    if (files.empty()) return;

    int totalFile = files.size();
    int blkSize = (totalFile + m_threads.size() - 1) / m_threads.size();

    m_ui.m_progressBar->setMaximum(totalFile);

    int startFile = 0;
    Q_FOREACH(QObject *obj, m_threads)
    {   
        CounterThread *td = qobject_cast<CounterThread*>(obj);
        int step = std::min(blkSize, totalFile - startFile);

        emit signal_filesQueued(files.mid(startFile, step), td);

        startFile += step;
    }
}

void DlgLineCounter::timerEvent(QTimerEvent * evt)
{
    m_ui.m_edtFileCount->setText(QString::number(m_ui.m_edtFileCount->property("value").toInt()));
    m_ui.m_edtLineCount->setText(QString::number(m_ui.m_edtLineCount->property("value").toInt()));
    m_ui.m_progressBar->setValue(m_ui.m_edtFileCount->property("value").toInt());
    m_ui.m_lblIteredFile->setText(m_ui.m_lblIteredFile->property("value").toString());
}

void DlgLineCounter::slot_lineCountOver(const QString& file, int lineCnt)
{
    m_ui.m_edtFileCount->setProperty("value", m_ui.m_edtFileCount->property("value").toInt() + 1);
    m_ui.m_edtLineCount->setProperty("value", m_ui.m_edtLineCount->property("value").toInt() + lineCnt);
    m_ui.m_lblIteredFile->setProperty(
        "value", 
        m_ui.m_edtFileCount->property("value").toInt() == m_ui.m_progressBar->maximum() ? tr("统计结束") : tr("统计: ") + file);
}