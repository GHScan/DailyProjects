using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace _5_CompilingByteCode {
    interface IASTNode {
        void AcceptVisitor(IASTNodeVisitor v);
    }

    class ASTNode_Literal: IASTNode {
        public object value;
        public void AcceptVisitor(IASTNodeVisitor v) { v.Visit(this); }
    }

    class ASTNode_GetVar: IASTNode {
        public object variable;
        public void AcceptVisitor(IASTNodeVisitor v) { v.Visit(this); }
    }

    class ASTNode_SetVar: IASTNode {
        public object variable;
        public IASTNode rightNode;
        public void AcceptVisitor(IASTNodeVisitor v) { v.Visit(this); }
    }

    class ASTNode_If: IASTNode {
        public IASTNode predNode, thenNode, elseNode;
        public void AcceptVisitor(IASTNodeVisitor v) { v.Visit(this); }
    }

    class ASTNode_Begin: IASTNode {
        public List<IASTNode> nodes;
        public void AcceptVisitor(IASTNodeVisitor v) { v.Visit(this); }
    }

    class ASTNode_Lambda: IASTNode {
        public IASTNode bodyNode;
        public int localVarCount;
        public List<FreeVariable> freeVariables;
        public void AcceptVisitor(IASTNodeVisitor v) { v.Visit(this); }
    }

    class ASTNode_Application: IASTNode {
        public IASTNode procedureNode;
        public List<IASTNode> actualNodes;
        public void AcceptVisitor(IASTNodeVisitor v) { v.Visit(this); }
    }

    interface IASTNodeVisitor {
        void Visit(ASTNode_Literal node);
        void Visit(ASTNode_GetVar node);
        void Visit(ASTNode_SetVar node);
        void Visit(ASTNode_If node);
        void Visit(ASTNode_Begin node);
        void Visit(ASTNode_Lambda node);
        void Visit(ASTNode_Application node);
    }
}
