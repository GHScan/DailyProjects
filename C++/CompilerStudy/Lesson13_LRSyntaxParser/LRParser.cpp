
#include "pch.h"

#include "LRParser.h"
#include "SLRParser.h"
#include "CLRParser.h"
#include "LALRParser.h"

IParserTable* IParserTable::create(const string& type)
{
    if (type == "SLR") return new SLRParserTable();
    else if (type == "CLR") return new CLRParserTable();
    else if (type == "LALR") return new LALRParserTable();
    return NULL;
}

static void lexicalAnalysiser(
        vector<string>& tokens, const string& _src)
{
    const char *src = _src.c_str();
    for (;;) {
        while (isspace(src[0])) ++src;
        if (src[0] == 0) break;
        tokens.push_back(string(src, src + 1));
        ++src;
    }
}

LRParser::LRParser(const string& type, const BNFInstance& bnf, ILROutput *output):
    m_table(IParserTable::create(type)), m_output(output)
{
    m_output->onBeginBuild();
    m_table->init(bnf);
    m_output->onEndBuild(m_table->getActionTable()->getStateCount());
}
LRParser::~LRParser()
{
    delete m_table;
}
void LRParser::parse(const string& src)
{
    m_output->onBeginParse();

    IGotoTable* gotoTable = m_table->getGotoTable();
    IActionTable* actionTable = m_table->getActionTable();
    const BNFInstance* bnf = m_table->getBNF();

    vector<int> stateStack;
    vector<string> inputTerms;
    lexicalAnalysiser(inputTerms, src);
    inputTerms.push_back(END_TERM);

    int ipos = 0;
    stateStack.push_back(m_table->getStartState());
    for (;;) {
        Action act = actionTable->getAction(stateStack.back(), inputTerms[ipos]);
        if (act.type == Action::T_Shift) {
            if (inputTerms[ipos] == END_TERM) {
                ++ipos;
                break;
            }
            m_output->onShift(inputTerms[ipos++]);
            stateStack.push_back(act.value);
        }
        else if (act.type == Action::T_Reduce) {
            const Production* production = bnf->ID2production[act.value];
            m_output->onReduce(*production);
            stateStack.erase(stateStack.end() - production->size() + 1, stateStack.end());
            int ns = gotoTable->getNextState(stateStack.back(), (*production)[0].value);
            stateStack.push_back(ns);
        }
        else assert(0);
    }

    m_output->onEndParse(ipos == inputTerms.size());
}
