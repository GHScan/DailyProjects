import java.util.*;

// ExprNode
interface IExprNodeVisitor {
    void visit(ExprNode_ConstNumber node);
    void visit(ExprNode_ConstString node);
    void visit(ExprNode_ID node);
    void visit(ExprNode_ArrayConstructor node);
    void visit(ExprNode_UnaryOp node);
    void visit(ExprNode_BinaryOp node);
    void visit(ExprNode_IndexOf node);
    void visit(ExprNode_Call node);
}

abstract class ExprNode {
    protected ExprNode(int _line) { line = _line; }
    public int line;
    public abstract void acceptVisitor(IExprNodeVisitor v);
}

class ExprNode_ConstNumber extends ExprNode {
    public ExprNode_ConstNumber(int _line, double _number) { super(_line); number = _number; }
    public double number;
    @Override
    public void acceptVisitor(IExprNodeVisitor v) { v.visit(this); }
}
class ExprNode_ConstString extends ExprNode {
    public ExprNode_ConstString(int _line, String _str) { super(_line); str = _str;}
    public String str;
    @Override
    public void acceptVisitor(IExprNodeVisitor v) { v.visit(this); }
}
class ExprNode_ID extends ExprNode { 
    public ExprNode_ID(int _line, String _name) { super(_line); name = _name;  }
    public String name;
    @Override
    public void acceptVisitor(IExprNodeVisitor v) { v.visit(this); }
}
class ExprNode_ArrayConstructor extends ExprNode {
    public ExprNode_ArrayConstructor(int _line, ArrayList<ExprNode> _exprs) { super(_line); exprs = _exprs; }
    public ArrayList<ExprNode> exprs;
    @Override
    public void acceptVisitor(IExprNodeVisitor v) { v.visit(this); }
}
class ExprNode_UnaryOp extends ExprNode {
    public ExprNode_UnaryOp(int _line, String _op, ExprNode _expr) { super(_line); op = _op; expr = _expr; }
    public String op;
    public ExprNode expr;
    @Override
    public void acceptVisitor(IExprNodeVisitor v) { v.visit(this); }
}
class ExprNode_BinaryOp extends ExprNode {
    public ExprNode_BinaryOp(int _line, String _op, ExprNode _lexpr, ExprNode _rexpr) { super(_line); op = _op; lexpr = _lexpr; rexpr = _rexpr; }
    public String op;
    public ExprNode lexpr, rexpr;
    @Override
    public void acceptVisitor(IExprNodeVisitor v) { v.visit(this); }
}
class ExprNode_IndexOf extends ExprNode {
    public ExprNode_IndexOf(int _line, ExprNode _arrayExpr, ExprNode _indexExpr) { super(_line); arrayExpr = _arrayExpr; indexExpr = _indexExpr; }
    public ExprNode arrayExpr,  indexExpr;
    @Override
    public void acceptVisitor(IExprNodeVisitor v) { v.visit(this); }
}
class ExprNode_Call extends ExprNode {
    public ExprNode_Call(int _line, ExprNode _funcExpr, ArrayList<ExprNode> _paramExprs) { super(_line); funcExpr = _funcExpr; paramExprs = _paramExprs; }
    public ExprNode funcExpr;
    public ArrayList<ExprNode> paramExprs;
    @Override 
    public void acceptVisitor(IExprNodeVisitor v) { v.visit(this); }
}

// StmtNode
interface IStmtNodeVisitor {
    void visit(StmtNode_DeclareLocal node);
    void visit(StmtNode_DeclareArg node);
    void visit(StmtNode_Block node);
    void visit(StmtNode_Stmts node);
    void visit(StmtNode_Assign node);
    void visit(StmtNode_Call node);
    void visit(StmtNode_IfElse node);
    void visit(StmtNode_For node);
    void visit(StmtNode_Continue node);
    void visit(StmtNode_Break node);
    void visit(StmtNode_Return node);
}

abstract class StmtNode {
    public abstract void acceptVisitor(IStmtNodeVisitor v);
    protected StmtNode(int _line) { line = _line;  }
    public int line;
}

class StmtNode_DeclareLocal extends StmtNode {
    public StmtNode_DeclareLocal(int _line, String _name) { super(_line); name = _name;  }
    public String name;
    @Override
    public void acceptVisitor(IStmtNodeVisitor v) { v.visit(this); }
}
class StmtNode_DeclareArg extends StmtNode {
    public StmtNode_DeclareArg(int _line, String _name) { super(_line); name = _name; }
    public String name;
    @Override
    public void acceptVisitor(IStmtNodeVisitor v) { v.visit(this); }
}
class StmtNode_Block extends StmtNode {
    public StmtNode_Block(int _line) { super(_line); stmts = new ArrayList<StmtNode>();}
    public ArrayList<StmtNode> stmts;
    @Override
    public void acceptVisitor(IStmtNodeVisitor v) { v.visit(this); }
}
class StmtNode_Stmts extends StmtNode {
    public StmtNode_Stmts(int _line) { super(_line); stmts = new ArrayList<StmtNode>();}
    public ArrayList<StmtNode> stmts;
    @Override
    public void acceptVisitor(IStmtNodeVisitor v) { v.visit(this); }
}
class StmtNode_Assign extends StmtNode {
    public StmtNode_Assign(int _line, ExprNode _lexpr, ExprNode _rexpr) { super(_line); lexpr = _lexpr; rexpr = _rexpr; }
    public ExprNode lexpr, rexpr;
    @Override
    public void acceptVisitor(IStmtNodeVisitor v) { v.visit(this); }
}
class StmtNode_Call extends StmtNode {
    public StmtNode_Call(int _line, ExprNode_Call _callExpr) { super(_line); callExpr = _callExpr; }
    public ExprNode_Call callExpr;
    @Override
    public void acceptVisitor(IStmtNodeVisitor v) { v.visit(this); }
}
class StmtNode_IfElse extends StmtNode {
    public StmtNode_IfElse(int _line, ExprNode _expr, StmtNode _ifStmt, StmtNode _elseStmt) { super(_line); expr = _expr; ifStmt = _ifStmt; elseStmt = _elseStmt; }
    public ExprNode expr;
    public StmtNode ifStmt, elseStmt;
    @Override
    public void acceptVisitor(IStmtNodeVisitor v) { v.visit(this); }
}
class StmtNode_For extends StmtNode {
    public StmtNode_For(int _line, StmtNode _first, ExprNode _second, StmtNode _third, StmtNode _body) { 
        super(_line);
        first = _first; second = _second; third = _third; body = _body; 
    }
    public StmtNode first, third, body;
    public ExprNode second;
    @Override
    public void acceptVisitor(IStmtNodeVisitor v) { v.visit(this); }
}
class StmtNode_Continue extends StmtNode {
    public StmtNode_Continue(int _line) { super(_line);}
    @Override
    public void acceptVisitor(IStmtNodeVisitor v) { v.visit(this); }
}
class StmtNode_Break extends StmtNode {
    public StmtNode_Break(int _line) { super(_line); }
    @Override
    public void acceptVisitor(IStmtNodeVisitor v) { v.visit(this); }
}
class StmtNode_Return extends StmtNode {
    public StmtNode_Return(int _line, ExprNode _expr)  { super(_line); expr = _expr; }
    public ExprNode expr;
    @Override
    public void acceptVisitor(IStmtNodeVisitor v) { v.visit(this); }
}
