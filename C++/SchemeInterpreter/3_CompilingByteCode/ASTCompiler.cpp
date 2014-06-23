#include "pch.h"
#include "ASTCompiler.h"
#include "STypes.h"
#include "SPairList.h"

static int getLiteralIndex(vector<SValue> &literals, SValue exp) {
    auto iter = find_if(literals.begin(), literals.end(), [exp](SValue lit){ return lit.equal(exp); });
    if (iter == literals.end()) {
        iter = literals.insert(literals.end(), exp);
    }
    return iter - literals.begin();
}

ASTNodePtr compileToAST(SymbolTable *symTable, vector<SValue> &literals, SValue exp) {
    int type = exp.getType();
    if (type == SVT_Symbol) {
        return make_shared<ASTNode_GetVar>(symTable->lookup(exp.getSymbol()->c_str()));

    } else if (type != SVT_Pair) {
        return make_shared<ASTNode_Literal>(getLiteralIndex(literals, exp));
    }

    SPairListAccessor list(exp.getObject()->staticCast<SPair>());
    int formID = list.ref<0>().getType() == SVT_Symbol ? list.ref<0>().getSymbol()->getID() : -1;
    switch (formID) {
        case SSymbol::ID_Quote:
            return make_shared<ASTNode_Literal>(getLiteralIndex(literals, list.ref<1>()));

        case SSymbol::ID_If: {
            return make_shared<ASTNode_If>(
                    compileToAST(symTable, literals, list.ref<1>()),
                    compileToAST(symTable, literals, list.ref<2>()),
                    compileToAST(symTable, literals, list.ref<3>()));
         }

        case SSymbol::ID_Begin: {
            auto beginNode = make_shared<ASTNode_Begin>();
            for (auto iter = list.begin(), end = list.end(); ++iter != end; ) {
                beginNode->nodes.push_back(compileToAST(symTable, literals, *iter));
            }
            return beginNode;
        }

        case SSymbol::ID_Lambda: {
            vector<string> formals;
            {
                SPairListAccessor list2(list.ref<1>().getObject()->staticCast<SPair>());
                for (auto sym : list2) {
                    formals.push_back(sym.getSymbol()->c_str());
                }
            }

            SymbolTable newSymbTable(symTable);
            return make_shared<ASTNode_Lambda>(move(formals), 
                    compileToAST(&newSymbTable, literals, list.ref<2>()));
         }

        case SSymbol::ID_Define: {
            return make_shared<ASTNode_Define>(list.ref<1>().getSymbol()->c_str(), 
                    compileToAST(symTable, literals, list.ref<2>()));
         }

        case SSymbol::ID_Set: {
            return make_shared<ASTNode_SetVar>(
                    symTable->lookup(list.ref<1>().getSymbol()->c_str()), 
                    compileToAST(symTable, literals, list.ref<2>()));
          }

        default: {
            vector<ASTNodePtr> exps;
            for (auto e : list) {
                exps.push_back(compileToAST(symTable, literals, e));
            }

            ASTNodePtr func = exps[0];
            exps.erase(exps.begin());

            return make_shared<ASTNode_Application>(func, move(exps));
         }
    }

    return nullptr;
}

