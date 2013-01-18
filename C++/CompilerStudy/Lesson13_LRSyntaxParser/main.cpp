#include "pch.h" 

#include <time.h>

#include "BNFParser.h"
#include "LRParser.h"

class LROutput_Print:
    public ILROutput
{
public:
    LROutput_Print(const string& name, const string& fname):
        m_fo(fname.c_str()), m_start(0)
    {
        m_fo << "LR " << name << endl;
    }
private:
    virtual void onBeginBuild()
    {
        m_start = clock();
    }
    virtual void onEndBuild(int stateCnt)
    {
        m_fo << "state count : " << stateCnt << endl;
        m_fo << "time : " << float(clock() - m_start) / CLOCKS_PER_SEC << endl;
    }
    virtual void onBeginParse()
    {
        m_start = clock();
    }
    virtual void onReduce(const Production& p)
    {
        //m_fo << "r: " << p << endl;
    }
    virtual void onShift(const string& term)
    {
        //m_fo << "s: " << term << endl;
    }
    virtual void onEndParse(bool success)
    {
        m_fo << (success ? "success!" : "failed!") << endl;
        m_fo << "time : " << float(clock() - m_start) / CLOCKS_PER_SEC << endl;
    }
private:
    ofstream m_fo;
    clock_t m_start;
};

string readSource(const string& fname)
{
    string src;
    ifstream fi(fname.c_str());
    for (string line; getline(fi, line); src += line + '\n') {
        if (line.size() >= 2 && line.find("//") == 0) line = "";
    }
    return src;
}

int main()
{
    BNFInstance bnf;
    {
        ifstream fi("syntax.y");
        bnf.parse(fi);
    }
    {
        ofstream fo("syntax2.y");
        bnf.dump(fo);
    }

    string types[] = {"SLR", "LALR", "CLR"};
    for (auto type : types) {
        LROutput_Print printer(type, "o_" + type + ".txt");
        LRParser parser(type, bnf, &printer);
        parser.parse(readSource("source.txt"));
    }
} 
