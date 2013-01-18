#include "stdafx.h"
#include "DlgLineCounter.h"
#include <QtGui/QApplication>

int main(int argc, char *argv[])
{
    QTextCodec::setCodecForTr(QTextCodec::codecForName("gb2312"));

    QApplication app(argc, argv);
    app.setApplicationName(app.objectName());

    DlgLineCounter w;
    w.show();

    int ret = app.exec();
    return ret;
}
