#include "pch.h"
#include "ASTCompiler.h"
#include "STypes.h"
#include "SPairList.h"

static int getLiteralIndex(vector<SValue> *literals, SValue exp) {
    auto iter = find_if(literals->begin(), literals->end(), [exp](SValue lit){ return lit.equal(exp); });
    if (iter == literals->end()) {
        iter = literals->insert(literals->end(), exp);
    }
    return iter - literals->begin();
}

static void findInternalDefines(SymbolTable *symTable, SValue exp) {
    if (exp.getType() != SPair::TYPE) return;

    SPairListAccessor list(exp);
    int formID = list.ref<0>().getType() == SVT_Symbol ? list.ref<0>().getSymbol()->getID() : -1;
    switch (formID) {
        case SSymbol::ID_Define:
            symTable->define(list.ref<1>().getSymbol()->c_str());
            break;
        case SSymbol::ID_Begin: {
                for (auto iter = list.begin(), end = list.end(); ++iter != end;) {
                    findInternalDefines(symTable, *iter);
                }
            }
            break;
        default:
            break;
    }
}

ASTNodePtr compileToAST(SymbolTable *symTable, vector<SValue> *literals, SValue exp) {
    int type = exp.getType();
    if (type == SVT_Symbol) {
        return make_shared<ASTNode_GetVar>(symTable->lookup(exp.getSymbol()->c_str()));

    } else if (type != SPair::TYPE) {
        return make_shared<ASTNode_Literal>(getLiteralIndex(literals, exp));
    }

    SPairListAccessor list(exp);
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

            SymbolTable newSymTable(symTable);

            {
                SPairListAccessor list2(list.ref<1>());
                for (auto sym : list2) {
                    newSymTable.define(sym.getSymbol()->c_str());
                }
            }
            int formalCount = newSymTable.getSymbolCount();

            findInternalDefines(&newSymTable, list.ref<2>());

            ASTNodePtr body = compileToAST(&newSymTable, literals, list.ref<2>());

            return make_shared<ASTNode_Lambda>(formalCount, newSymTable.getSymbols(), body);
         }

        case SSymbol::ID_Define: {
            return make_shared<ASTNode_SetVar>(
                    symTable->lookup(list.ref<1>().getSymbol()->c_str()), 
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

