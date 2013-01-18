#ifndef DLGLINECOUNTER_H
#define DLGLINECOUNTER_H

#include <QtGui/QDialog>
#include "ui_DlgLineCounter.h"

class DlgLineCounter: 
    public QDialog
{
    Q_OBJECT

public:
    DlgLineCounter(QWidget *parent = 0, Qt::WFlags flags = 0);
    ~DlgLineCounter();

protected:
    virtual void dragEnterEvent(QDragEnterEvent *evt);
    virtual void dropEvent(QDropEvent *evt);
    virtual void timerEvent(QTimerEvent * evt);

signals:
    void signal_filesQueued(const QStringList& files, QObject *worker);
    void signal_startCountLine();

protected slots:
    void slot_lineCountOver(const QString& file, int lineCnt);

private:
    Ui::DlgLineCounterClass m_ui;
    QVector<QObject*>       m_threads;
};

#endif // DLGLINECOUNTER_H
