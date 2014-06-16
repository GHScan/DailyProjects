using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace _5_CompilingByteCode {
    class ASTNodeVisitor_FindFreeVariables: IASTNodeVisitor {
        public List<FreeVariable> FreeVariables { get; private set; }
        public ASTNodeVisitor_FindFreeVariables(IASTNode node) {
            FreeVariables = new List<FreeVariable>();
            node.AcceptVisitor(this);
        }
        public void Visit(ASTNode_Literal node) {

        }
        public void Visit(ASTNode_GetVar node) {
            var v = node.variable as FreeVariable;
            if (v != null && FreeVariables.IndexOf(v) == -1) {
                FreeVariables.Add(v);
            }
        }
        public void Visit(ASTNode_SetVar node) {
            var v = node.variable as FreeVariable;
            if (v != null && FreeVariables.IndexOf(v) == -1) {
                FreeVariables.Add(v);
            }
            node.rightNode.AcceptVisitor(this);
        }
        public void Visit(ASTNode_If node) {
            node.predNode.AcceptVisitor(this);
            node.thenNode.AcceptVisitor(this);
            node.elseNode.AcceptVisitor(this);
        }
        public void Visit(ASTNode_Begin node) {
            foreach (var n in node.nodes) n.AcceptVisitor(this);
        }
        public void Visit(ASTNode_Lambda node) {
            node.bodyNode.AcceptVisitor(this);
        }
        public void Visit(ASTNode_Application node) {
            node.procedureNode.AcceptVisitor(this);
            foreach (var n in node.actualNodes) n.AcceptVisitor(this);
        }
    }

    class ASTCompiler {
        static public IASTNode Compile(SymbolTable symTable, object exp) {
            if (exp is string) {
                return new ASTNode_GetVar { variable = SymbolTable.Lookup(symTable, (string)exp) };
            } else if (exp is INumber) {
                return new ASTNode_Literal { value = exp };
            }

            List<object> form = exp as List<object>;
            switch (form[0] as string) {
                case "quote":
                    return new ASTNode_Literal { value = ListProcess.ListExpToPairExp(form[1]) };
                case "if":
                    return new ASTNode_If { predNode = Compile(symTable, form[1]), thenNode = Compile(symTable, form[2]), elseNode = Compile(symTable, form[3]) };
                case "begin":
                    return new ASTNode_Begin { nodes = form.Skip(1).Select(e => Compile(symTable, e)).ToList() };
                case "lambda": {
                        SymbolTable newSymTable = new SymbolTable(symTable);

                        foreach (var name in ((List<object>)form[1]).Cast<string>()) {
                            newSymTable.DefineLocalSymbol(name);
                        }

                        List<string> defines = new List<string>();
                        ListProcess.FindDefinition(defines, (List<object>)form[2], 1);
                        foreach (var name in defines) {
                            newSymTable.DefineLocalSymbol(name);
                        }

                        IASTNode body = Compile(newSymTable, form[2]);

                        return new ASTNode_Lambda {
                            localVarCount = newSymTable.GetLocalSymbolCount(),
                            bodyNode = body,
                            freeVariables = new ASTNodeVisitor_FindFreeVariables(body).FreeVariables,
                        };
                    }
                case "define":
                case "set!":
                    return new ASTNode_SetVar { variable = SymbolTable.Lookup(symTable, (string)form[1]), rightNode = Compile(symTable, form[2]) };
                default:
                    return new ASTNode_Application {
                        procedureNode = Compile(symTable, form[0]),
                        actualNodes = form.Skip(1).Select(e => Compile(symTable, e)).ToList(),
                    };
            }
        }
    }       
}
