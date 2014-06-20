using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace _6_JIT {
    class ASTNodeVisitor_FindFreeAddresses: IASTNodeVisitor {
        public HashSet<FreeAddress> BodyFreeAddresses { get; private set; }
        public HashSet<FreeAddress> ChildrenFreeAddresses { get; private set; }
        public ASTNodeVisitor_FindFreeAddresses(IASTNode node) {
            BodyFreeAddresses = new HashSet<FreeAddress>();
            ChildrenFreeAddresses = new HashSet<FreeAddress>();
            node.AcceptVisitor(this);
        }
        public void Visit(ASTNode_Literal node) {

        }
        public void Visit(ASTNode_GetVar node) {
            if (node.address is FreeAddress) {
                BodyFreeAddresses.Add((FreeAddress)node.address);
            }
        }
        public void Visit(ASTNode_SetVar node) {
            if (node.address is FreeAddress) {
                BodyFreeAddresses.Add((FreeAddress)node.address);
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
            ChildrenFreeAddresses.UnionWith(node.GetFreeAddresses().Select(
                address => new FreeAddress { envIndex=address.envIndex -1, index=address.index}));
        }
        public void Visit(ASTNode_Application node) {
            node.procedureNode.AcceptVisitor(this);
            foreach (var n in node.actualNodes) n.AcceptVisitor(this);
        }
    }

    class ASTCompiler {

        static public IASTNode Compile(SymbolTable symTable, object exp) {
            if (exp is string) {
                return new ASTNode_GetVar { address = SymbolTable.Lookup(symTable, (string)exp) };
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

                        var formals = ((List<object>)form[1]).Cast<string>();
                        foreach (var name in formals) {
                            newSymTable.DefineLocalSymbol(name);
                        }

                        List<string> defines = new List<string>();
                        ListProcess.FindDefinition(defines, (List<object>)form[2], 1);
                        foreach (var name in defines) {
                            newSymTable.DefineLocalSymbol(name);
                        }

                        IASTNode body = Compile(newSymTable, form[2]);
                        var finder = new ASTNodeVisitor_FindFreeAddresses(body);

                        return new ASTNode_Lambda {
                            formalCount = ((List<object>)form[1]).Count,
                            locals = formals.Concat(defines).ToList(),
                            bodyNode = body,
                            bodyFreeAddresses = finder.BodyFreeAddresses.ToList(),
                            childrenFreeAddresses = finder.ChildrenFreeAddresses.ToList(),
                        };
                    }
                case "define":
                case "set!":
                    return new ASTNode_SetVar { address = SymbolTable.Lookup(symTable, (string)form[1]), rightNode = Compile(symTable, form[2]) };
                default:
                    return new ASTNode_Application {
                        procedureNode = Compile(symTable, form[0]),
                        actualNodes = form.Skip(1).Select(e => Compile(symTable, e)).ToList(),
                    };
            }
        }
    }       
}
