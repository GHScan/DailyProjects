// Console.cpp : 定义控制台应用程序的入口点。
//

#include "stdafx.h"

#include <boost/shared_ptr.hpp>
#include <boost/regex.hpp>

class MyDialog:
    public QDialog
{
    Q_OBJECT
public:
    MyDialog();

private slots:
    void onTextChanged_expEdit();
    void onTextChanged_TestEdit();

private:
    QTextEdit   m_expEdit;
    QTextEdit   m_testEdit;
    QTextEdit   m_outEdit;
    boost::shared_ptr<boost::regex> m_regex;
};

#include "moc/console.cpp.cpp"

MyDialog::MyDialog()
{
    setWindowTitle("Test for : POSIX Extended Regular Expression Syntax");
    setMinimumSize(512, 384);

    connect(&m_expEdit, SIGNAL(textChanged()), SLOT(onTextChanged_expEdit()));
    connect(&m_testEdit, SIGNAL(textChanged()), SLOT(onTextChanged_TestEdit()));

    m_outEdit.setReadOnly(true);

    m_expEdit.setTabChangesFocus(true);
    m_testEdit.setTabChangesFocus(true);

    QVBoxLayout *layout = new QVBoxLayout(this);
    layout->addWidget(&m_expEdit, 3);
    layout->addWidget(&m_testEdit, 3);
    layout->addWidget(&m_outEdit, 5);
    QSizePolicy sizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
    m_expEdit.setSizePolicy(sizePolicy);
    m_testEdit.setSizePolicy(sizePolicy);
    m_outEdit.setSizePolicy(sizePolicy);
}

void MyDialog::onTextChanged_expEdit()
{
    try
    {
        m_regex.reset();
        m_regex.reset(new boost::regex(m_expEdit.toPlainText().toStdString()));
        m_outEdit.setText("valid regular expression");
    }
    catch(std::exception& e)
    {
        m_outEdit.setText(e.what());
    }
}

void MyDialog::onTextChanged_TestEdit()
{
    if (m_regex == NULL) return;

    m_outEdit.setText(
        boost::regex_match(m_testEdit.toPlainText().toStdString(), *m_regex) ? 
        "match" : "unmatch");
}

#include <windows.h>
int WINAPI WinMain(__in HINSTANCE hInstance,
        __in_opt HINSTANCE hPrevInstance,
        __in_opt LPSTR lpCmdLine,
        __in int nShowCmd)
{
    QTextCodec::setCodecForCStrings(QTextCodec::codecForLocale());

    int argc = 0;
    char *argv[] = {""};
    QApplication app(argc, argv);

    MyDialog dlg;
    dlg.show();

    return app.exec();
}