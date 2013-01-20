
#include "pch.h"
#include "CodeGen.h"
#include "UnitTest.h"
#include "RegParser.h"
#include "Scanner.h"

void regParserTest()
{
    puts("UnitTest----------> regParserTest");
    const char *regs[] = {
        "\\/\\/.*\\n",
        "\\t\\n",
        "[abc]",
        "[^abc]",
        "[^abc]|def",
        ".a.?",
        "([^abc]|def)?(ccc)*",
        "\\a+://[^\\s]*",
        "\\d+\\.\\d+\\.\\d+\\.\\d+",
        "[\\a_][\\w_]*|[1-9][0-9]*\\.?[0-9]*",
        "(a|b)*abb",
        "[\\a_][\\w_]*|[1-9][0-9]*\\.?[0-9]*",
    };
    for (auto reg : regs) {
        cout << 
            RegNodeVisitor_Printer().apply(RegParser(reg).getRoot()) << endl;
    }
}

void scannerTest()
{
    puts("UnitTest----------> scannerTest");
    Timer _timer1("build + parse + read");
    string str(readFile("source.js"));
    Timer _timer2("build + parse");
    Scanner s(str);
    Timer _timer3("parse");
    int n = 0;
    Token t;
    while (s.getNext(t)) {
        // cout << "(" << t.type << ",'" << t.value << "'),";
        ++n;
    }
    cout << "t count :" << n << endl;
}

void unitTest()
{
    // regParserTest();
    scannerTest();
}
