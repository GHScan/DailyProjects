#include "pch.h" 

#include <fstream>

#include "BNFParser.h"

int main()
{
    BNFStruct bnf;
    std::ifstream("1.txt") >> bnf;
    optimizeBNF(bnf);
    std::ofstream("2.txt") << bnf;
    PredictiveAnalysisTable(bnf).dump(std::ofstream("3.txt"));
}
