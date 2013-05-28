
using System.Collections.Generic;

// ExprNode
public interface IExprNodeVisitor {
    void visit(ExprNode_ConstNumber node);
    void visit(ExprNode_ConstString node);
    void visit(ExprNode_ID node);
    void visit(ExprNode_ArrayConstructor node);
    void visit(ExprNode_UnaryOp node);
    void visit(ExprNode_BinaryOp node);
    void visit(ExprNode_IndexOf node);
    void visit(ExprNode_Call node);
}

public abstract class ExprNode {
    protected ExprNode(int line) { Line = line; }
    public int Line { get; private set; }
    public abstract void acceptVisitor(IExprNodeVisitor v);
}

public class ExprNode_ConstNumber : ExprNode {
    public ExprNode_ConstNumber(int line, double number) : base(line) { Number = number; }
    public double Number { get; private set; }
    public override void acceptVisitor(IExprNodeVisitor v) { v.visit(this); }
}
public class ExprNode_ConstString : ExprNode {
    public ExprNode_ConstString(int line, string str) : base(line) { Str = str;}
    public string Str { get; private set; }
    public override void acceptVisitor(IExprNodeVisitor v) { v.visit(this); }
}
public class ExprNode_ID : ExprNode { 
    public ExprNode_ID(int line, string name) : base(line) { Name = name;  }
    public string Name { get; private set; }
    public override void acceptVisitor(IExprNodeVisitor v) { v.visit(this); }
}
public class ExprNode_ArrayConstructor : ExprNode {
    public ExprNode_ArrayConstructor(int line, List<ExprNode> exprs) : base(line) { Exprs = exprs; }
    public List<ExprNode> Exprs { get; private set; } 
    public override void acceptVisitor(IExprNodeVisitor v) { v.visit(this); }
}
public class ExprNode_UnaryOp : ExprNode {
    public ExprNode_UnaryOp(int line, string op, ExprNode expr) : base(line) { Op = op; Expr = expr; }
    public string Op { get; private set; }
    public ExprNode Expr { get; private set; }
    public override void acceptVisitor(IExprNodeVisitor v) { v.visit(this); }
}
public class ExprNode_BinaryOp : ExprNode {
    public ExprNode_BinaryOp(int line, string op, ExprNode lexpr, ExprNode rexpr) : base(line) { Op = op; Lexpr = lexpr; Rexpr = rexpr; }
    public string Op { get; private set; }
    public ExprNode Lexpr { get; private set; }
    public ExprNode Rexpr { get; private set; }
    public override void acceptVisitor(IExprNodeVisitor v) { v.visit(this); }
}
public class ExprNode_IndexOf : ExprNode {
    public ExprNode_IndexOf(int line, ExprNode arrayExpr, ExprNode indexExpr):base(line) { ArrayExpr = arrayExpr; IndexExpr = indexExpr; }
    public ExprNode ArrayExpr { get; private set; }
    public ExprNode IndexExpr { get; private set; }
    public override void acceptVisitor(IExprNodeVisitor v) { v.visit(this); }
}
public class ExprNode_Call : ExprNode {
    public ExprNode_Call(int line, ExprNode funcExpr, List<ExprNode> paramExprs) : base(line) { FuncExpr = funcExpr; ParamExprs = paramExprs; }
    public ExprNode FuncExpr { get; private set; }
    public List<ExprNode> ParamExprs { get; private set; }
    public override void acceptVisitor(IExprNodeVisitor v) { v.visit(this); }
}

// StmtNode
public interface IStmtNodeVisitor {
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

public abstract class StmtNode {
    public abstract void acceptVisitor(IStmtNodeVisitor v);
    protected StmtNode(int line) { Line = line;  }
    public int Line { get; set; }
}

public class StmtNode_DeclareLocal : StmtNode {
    public StmtNode_DeclareLocal(int line, string name) : base(line) { Name = name;  }
    public string Name { get; private set; }
    public override void acceptVisitor(IStmtNodeVisitor v) { v.visit(this); }
}
public class StmtNode_DeclareArg : StmtNode {
    public StmtNode_DeclareArg(int line, string name): base(line) { Name = name; }
    public string Name { get; private set; }
    public override void acceptVisitor(IStmtNodeVisitor v) { v.visit(this); }
}
public class StmtNode_Block: StmtNode {
    public StmtNode_Block(int line) : base(line) { Stmts = new List<StmtNode>();}
    public List<StmtNode> Stmts { get; private set; }
    public override void acceptVisitor(IStmtNodeVisitor v) { v.visit(this); }
}
public class StmtNode_Stmts: StmtNode {
    public StmtNode_Stmts(int line) : base(line) { Stmts = new List<StmtNode>();}
    public List<StmtNode> Stmts { get; private set; }
    public override void acceptVisitor(IStmtNodeVisitor v) { v.visit(this); }
}
public class StmtNode_Assign: StmtNode {
    public StmtNode_Assign(int line, ExprNode lexpr, ExprNode rexpr) : base(line) { Lexpr = lexpr; Rexpr = rexpr; }
    public ExprNode Lexpr { get; private set; }
    public ExprNode Rexpr { get; private set; }
    public override void acceptVisitor(IStmtNodeVisitor v) { v.visit(this); }
}
public class StmtNode_Call: StmtNode {
    public StmtNode_Call(int line, ExprNode_Call callExpr):base(line) { CallExpr = callExpr; }
    public ExprNode_Call CallExpr { get;private set; }
    public override void acceptVisitor(IStmtNodeVisitor v) { v.visit(this); }
}
public class StmtNode_IfElse : StmtNode {
    public StmtNode_IfElse(int line, ExprNode expr, StmtNode ifStmt, StmtNode elseStmt) : base(line) { Expr = expr; IfStmt = ifStmt; ElseStmt = elseStmt; }
    public ExprNode Expr { get; private set; }
    public StmtNode IfStmt { get; private set; }
    public StmtNode ElseStmt { get; private set; }
    public override void acceptVisitor(IStmtNodeVisitor v) { v.visit(this); }
}
public class StmtNode_For : StmtNode {
    public StmtNode_For(int line, StmtNode first, ExprNode second, StmtNode third, StmtNode body) : base(line) { First = first; Second = second; Third = third; Body = body; }
    public StmtNode First { get; private set; }
    public ExprNode Second { get; private set; }
    public StmtNode Third { get; private set; }
    public StmtNode Body { get; private set; }
    public override void acceptVisitor(IStmtNodeVisitor v) { v.visit(this); }
}
public class StmtNode_Continue : StmtNode {
    public StmtNode_Continue(int line): base(line) { }
    public override void acceptVisitor(IStmtNodeVisitor v) { v.visit(this); }
}
public class StmtNode_Break : StmtNode {
    public StmtNode_Break(int line) : base(line) { }
    public override void acceptVisitor(IStmtNodeVisitor v) { v.visit(this); }
}
public class StmtNode_Return : StmtNode {
    public StmtNode_Return(int line, ExprNode expr) : base(line) { Expr = expr; }
    public ExprNode Expr { get; private set; }
    public override void acceptVisitor(IStmtNodeVisitor v) { v.visit(this); }
}
