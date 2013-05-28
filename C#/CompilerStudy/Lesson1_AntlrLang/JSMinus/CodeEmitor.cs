
using System;
using System.Collections.Generic;
using System.Linq;

using System.Diagnostics;
using System.Reflection;
using System.Reflection.Emit;

public class ExprNodeVisitor_CodeEmitor : IExprNodeVisitor {
    public ExprNodeVisitor_CodeEmitor(StmtNodeVisitor_CodeEmitor stmt, ExprNode node) {
        m_stmt = stmt;
        node.acceptVisitor(this);
    }
    public void toBoolean() {
        // optimize
        var doubleLabel = m_stmt.ILGenerator.DefineLabel();
        var endLabel = m_stmt.ILGenerator.DefineLabel();
        m_stmt.ILGenerator.Emit(OpCodes.Dup);
        m_stmt.ILGenerator.Emit(OpCodes.Isinst, typeof(double));
        m_stmt.ILGenerator.Emit(OpCodes.Ldnull);
        m_stmt.ILGenerator.Emit(OpCodes.Cgt_Un);
        m_stmt.ILGenerator.Emit(OpCodes.Brtrue, doubleLabel);
        m_stmt.ILGenerator.Emit(OpCodes.Ldnull);
        m_stmt.ILGenerator.Emit(OpCodes.Cgt_Un);
        m_stmt.ILGenerator.Emit(OpCodes.Br, endLabel);
        m_stmt.ILGenerator.MarkLabel(doubleLabel);
        m_stmt.ILGenerator.Emit(OpCodes.Unbox_Any, typeof(double));
        m_stmt.ILGenerator.Emit(OpCodes.Ldc_R8, 0.0);
        m_stmt.ILGenerator.Emit(OpCodes.Ceq);
        m_stmt.ILGenerator.Emit(OpCodes.Ldc_I4_0);
        m_stmt.ILGenerator.Emit(OpCodes.Ceq);
        m_stmt.ILGenerator.MarkLabel(endLabel);
    }
    public void toInt32() {
        m_stmt.ILGenerator.Emit(OpCodes.Unbox_Any, typeof(double));
        m_stmt.ILGenerator.Emit(OpCodes.Conv_I4);
    }

    public void visit(ExprNode_ConstNumber node) {
        m_stmt.ILGenerator.Emit(OpCodes.Ldc_R8, node.Number);
        m_stmt.ILGenerator.Emit(OpCodes.Box, typeof(double));
    }
    public void visit(ExprNode_ConstString node) {
        if (node.Str == null) m_stmt.ILGenerator.Emit(OpCodes.Ldnull);
        else m_stmt.ILGenerator.Emit(OpCodes.Ldstr, node.Str);
     }
    public void visit(ExprNode_ID node) {
        int argIdx = m_stmt.getArg(node.Name);
        if (argIdx != -1) {
            switch (argIdx) {
                case 0: m_stmt.ILGenerator.Emit(OpCodes.Ldarg_0);  break;
                case 1: m_stmt.ILGenerator.Emit(OpCodes.Ldarg_1); break;
                case 2: m_stmt.ILGenerator.Emit(OpCodes.Ldarg_2); break;
                case 3: m_stmt.ILGenerator.Emit(OpCodes.Ldarg_3); break;
                default:
                    if (argIdx < 256) m_stmt.ILGenerator.Emit(OpCodes.Ldarg_S, argIdx);
                    else m_stmt.ILGenerator.Emit(OpCodes.Ldarg, argIdx);
                    break;
            }
            return;
        }
        var local = m_stmt.getLocal(node.Name);
        if (local != null) {
            // optimize ????
            m_stmt.ILGenerator.Emit(OpCodes.Ldloc, local);
            return;
        }

        m_stmt.ILGenerator.Emit(OpCodes.Ldsfld, m_stmt.Emitor.GlobalField);
        m_stmt.ILGenerator.Emit(OpCodes.Ldstr, node.Name);
        m_stmt.ILGenerator.Emit(OpCodes.Callvirt, typeof(Dictionary<string, Object>).GetMethod("get_Item"));
    }
    public void visit(ExprNode_ArrayConstructor node) {
        m_stmt.ILGenerator.Emit(OpCodes.Newobj, typeof(List<Object>).GetConstructor(new Type[]{}));
        foreach (var expr in node.Exprs) {
            m_stmt.ILGenerator.Emit(OpCodes.Dup);
            new ExprNodeVisitor_CodeEmitor(m_stmt, expr);
            m_stmt.ILGenerator.Emit(OpCodes.Callvirt, typeof(List<Object>).GetMethod("Add"));
        }
    }
    public void visit(ExprNode_UnaryOp node) {
        var expr = new ExprNodeVisitor_CodeEmitor(m_stmt, node.Expr);
        switch (node.Op) { 
            case "not":
                expr.toBoolean();
                m_stmt.ILGenerator.Emit(OpCodes.Ldc_I4_0);
                m_stmt.ILGenerator.Emit(OpCodes.Ceq);
                break;
            case "-":
                m_stmt.ILGenerator.Emit(OpCodes.Unbox_Any, typeof(double));
                m_stmt.ILGenerator.Emit(OpCodes.Neg);
                m_stmt.ILGenerator.Emit(OpCodes.Box, typeof(double));
                break;
            case "#":
                m_stmt.ILGenerator.Emit(OpCodes.Callvirt, typeof(List<Object>).GetMethod("get_Count"));
                m_stmt.ILGenerator.Emit(OpCodes.Conv_R8);
                m_stmt.ILGenerator.Emit(OpCodes.Box, typeof(double));
                break;
            default: Trace.Assert(false);  break;
        }
    }
    public void visit(ExprNode_BinaryOp node) {
        var lexpr = new ExprNodeVisitor_CodeEmitor(m_stmt, node.Lexpr);
        if (node.Op == "&&" || node.Op == "||") {
            var endLabel = m_stmt.ILGenerator.DefineLabel();
            lexpr.toBoolean();
            m_stmt.ILGenerator.Emit(OpCodes.Dup);
            if (node.Op == "&&") m_stmt.ILGenerator.Emit(OpCodes.Brfalse, endLabel);
            else m_stmt.ILGenerator.Emit(OpCodes.Brtrue, endLabel);
            m_stmt.ILGenerator.Emit(OpCodes.Pop);
            new ExprNodeVisitor_CodeEmitor(m_stmt, node.Rexpr).toBoolean();
            m_stmt.ILGenerator.MarkLabel(endLabel);
            return;
        }

        if (node.Op == "==" || node.Op == "!=") {
            new ExprNodeVisitor_CodeEmitor(m_stmt, node.Rexpr);
            m_stmt.ILGenerator.Emit(OpCodes.Ceq);
            if (node.Op == "!=") {
                m_stmt.ILGenerator.Emit(OpCodes.Ldc_I4_0);
                m_stmt.ILGenerator.Emit(OpCodes.Ceq);
            }
            return;
        }

        m_stmt.ILGenerator.Emit(OpCodes.Unbox_Any, typeof(double));
        new ExprNodeVisitor_CodeEmitor(m_stmt, node.Rexpr);
        m_stmt.ILGenerator.Emit(OpCodes.Unbox_Any, typeof(double));
        switch (node.Op) { 
            case "+":
                m_stmt.ILGenerator.Emit(OpCodes.Add);
                break;
            case "-":
                m_stmt.ILGenerator.Emit(OpCodes.Sub);
                break;
            case "*":
                m_stmt.ILGenerator.Emit(OpCodes.Mul);
                break;
            case "/":
                m_stmt.ILGenerator.Emit(OpCodes.Div);
                break;
            case "%":
                m_stmt.ILGenerator.Emit(OpCodes.Rem);
                break;
            case "<":
                m_stmt.ILGenerator.Emit(OpCodes.Clt);
                m_stmt.ILGenerator.Emit(OpCodes.Conv_R8);
                break;
            case "<=":
                m_stmt.ILGenerator.Emit(OpCodes.Cgt);
                m_stmt.ILGenerator.Emit(OpCodes.Not);
                m_stmt.ILGenerator.Emit(OpCodes.Conv_R8);
                break;
            case ">":
                m_stmt.ILGenerator.Emit(OpCodes.Cgt);
                m_stmt.ILGenerator.Emit(OpCodes.Conv_R8);
                break;
            case ">=":
                m_stmt.ILGenerator.Emit(OpCodes.Clt);
                m_stmt.ILGenerator.Emit(OpCodes.Not);
                m_stmt.ILGenerator.Emit(OpCodes.Conv_R8);
                break;
            default: Trace.Assert(false); break;
        }
        m_stmt.ILGenerator.Emit(OpCodes.Box, typeof(double));
    }
    public void visit(ExprNode_IndexOf node) {
        new ExprNodeVisitor_CodeEmitor(m_stmt, node.ArrayExpr);
        new ExprNodeVisitor_CodeEmitor(m_stmt, node.IndexExpr).toInt32();
        m_stmt.ILGenerator.Emit(OpCodes.Callvirt, typeof(List<Object>).GetMethod("get_Item"));
    }
    public void visit(ExprNode_Call node) {
        var funcName = (node.FuncExpr as ExprNode_ID).Name;
        foreach (var expr in node.ParamExprs) {
            new ExprNodeVisitor_CodeEmitor(m_stmt, expr);
        }
        if (funcName == "println") {
            m_stmt.ILGenerator.Emit(OpCodes.Call, typeof(System.Console).GetMethod("WriteLine", new Type[]{typeof(Object)}));
            m_stmt.ILGenerator.Emit(OpCodes.Ldnull);
        } else {
            m_stmt.ILGenerator.Emit(OpCodes.Call, m_stmt.Emitor.FuncName2MethodBuilder[funcName]);
        }
    }
    private StmtNodeVisitor_CodeEmitor m_stmt;
}

public class StmtNodeVisitor_CodeEmitor : IStmtNodeVisitor {
    public StmtNodeVisitor_CodeEmitor(CodeEmitor emitor, FuncMeta meta) {
        Emitor = emitor;
        FuncMeta = meta;
        var methodBuilder = emitor.TypeBuilder.DefineMethod(meta.Name, MethodAttributes.Static | MethodAttributes.Public, CallingConventions.Standard,
            meta.Name == "Main" ? typeof(int) : typeof(Object), Enumerable.Repeat(typeof(Object), meta.ArgCount).ToArray());
        ILGenerator = methodBuilder.GetILGenerator();
        emitor.FuncName2MethodBuilder[meta.Name] = methodBuilder;
    }
    public void emit() {
        m_retLabel = ILGenerator.DefineLabel();

        FuncMeta.Stmt.acceptVisitor(this);

        if (FuncMeta.Name == "Main") ILGenerator.Emit(OpCodes.Ldc_I4_0);
        else ILGenerator.Emit(OpCodes.Ldnull);
        ILGenerator.MarkLabel(m_retLabel);
        ILGenerator.Emit(OpCodes.Ret);
    }
    public CodeEmitor Emitor { get; private set; }
    public ILGenerator ILGenerator { get; private set; }
    public FuncMeta FuncMeta { get; private set; }

    public void visit(StmtNode_DeclareLocal node) {
        Trace.Assert(!m_locals[m_locals.Count - 1].ContainsKey(node.Name));
        var local = ILGenerator.DeclareLocal(typeof(Object));
        //local.SetLocalSymInfo(node.Name);
        m_locals[m_locals.Count - 1][node.Name] = local;
    }
    public void visit(StmtNode_DeclareArg node) {
        Trace.Assert(!m_args.ContainsKey(node.Name));
        m_args[node.Name] = m_args.Count;
    }
    public void visit(StmtNode_Block node) {
        m_locals.Add(new Dictionary<string, LocalBuilder>());
        foreach (var stmt in node.Stmts) stmt.acceptVisitor(this);
        m_locals.RemoveAt(m_locals.Count - 1);
    }
    public void visit(StmtNode_Stmts node) {
        foreach (var stmt in node.Stmts) stmt.acceptVisitor(this);
    }
    public void visit(StmtNode_Assign node) {
        var indexOfExpr = node.Lexpr as ExprNode_IndexOf;
        if (indexOfExpr != null) {
            new ExprNodeVisitor_CodeEmitor(this, indexOfExpr.ArrayExpr);
            new ExprNodeVisitor_CodeEmitor(this, indexOfExpr.IndexExpr).toInt32();
            new ExprNodeVisitor_CodeEmitor(this, node.Rexpr);
            ILGenerator.Emit(OpCodes.Callvirt, typeof(List<Object>).GetMethod("set_Item"));
        } else {
            var idExpr = node.Lexpr as ExprNode_ID;
            Trace.Assert(idExpr != null);

            var argIdx = getArg(idExpr.Name);
            if (argIdx != -1) {
                new ExprNodeVisitor_CodeEmitor(this, node.Rexpr);
                if (argIdx < 256) ILGenerator.Emit(OpCodes.Starg_S, argIdx);
                else ILGenerator.Emit(OpCodes.Starg, argIdx);
                return;
            }
            var local = getLocal(idExpr.Name);
            if (local != null) {
                new ExprNodeVisitor_CodeEmitor(this, node.Rexpr);
                ILGenerator.Emit(OpCodes.Stloc, local);
                return;
            }

            ILGenerator.Emit(OpCodes.Ldsfld, Emitor.GlobalField);
            ILGenerator.Emit(OpCodes.Ldstr, idExpr.Name);
            new ExprNodeVisitor_CodeEmitor(this, node.Rexpr);
            ILGenerator.Emit(OpCodes.Callvirt, typeof(Dictionary<string, Object>).GetMethod("set_Item"));
        }
    }
    public void visit(StmtNode_Call node) {
        new ExprNodeVisitor_CodeEmitor(this, node.CallExpr);
        ILGenerator.Emit(OpCodes.Pop);
    }
    public void visit(StmtNode_IfElse node) {
        var elseLabel = ILGenerator.DefineLabel();
        var endLabel = ILGenerator.DefineLabel();
        new ExprNodeVisitor_CodeEmitor(this, node.Expr).toBoolean();
        ILGenerator.Emit(OpCodes.Brfalse, elseLabel);
        if (node.IfStmt != null) node.IfStmt.acceptVisitor(this);
        ILGenerator.Emit(OpCodes.Br, endLabel);
        ILGenerator.MarkLabel(elseLabel);
        if (node.ElseStmt != null) node.ElseStmt.acceptVisitor(this);
        ILGenerator.MarkLabel(endLabel);
    }
    public void visit(StmtNode_For node) {
        if (node.First != null) node.First.acceptVisitor(this);

        var loopLabel = ILGenerator.DefineLabel();
        var breakLabel = ILGenerator.DefineLabel();
        var continueLabel = ILGenerator.DefineLabel();
        m_breakLabels.Add(breakLabel);
        m_continueLabels.Add(continueLabel);

        ILGenerator.MarkLabel(loopLabel);
        new ExprNodeVisitor_CodeEmitor(this, node.Second).toBoolean();
        ILGenerator.Emit(OpCodes.Brfalse, breakLabel);
        if (node.Body != null) node.Body.acceptVisitor(this);
        ILGenerator.MarkLabel(continueLabel);
        if (node.Third != null) node.Third.acceptVisitor(this);
        ILGenerator.Emit(OpCodes.Br, loopLabel);
        ILGenerator.MarkLabel(breakLabel);

        m_continueLabels.RemoveAt(m_continueLabels.Count - 1);
        m_breakLabels.RemoveAt(m_breakLabels.Count - 1);
    }
    public void visit(StmtNode_Continue node) {
        ILGenerator.Emit(OpCodes.Br, m_continueLabels[m_continueLabels.Count - 1]);
    }
    public void visit(StmtNode_Break node) {
        ILGenerator.Emit(OpCodes.Br, m_breakLabels[m_breakLabels.Count - 1]);
    }
    public void visit(StmtNode_Return node) {
        if (node.Expr == null) ILGenerator.Emit(OpCodes.Ldnull);
        else {
            new ExprNodeVisitor_CodeEmitor(this, node.Expr);
        }
        ILGenerator.Emit(OpCodes.Br, m_retLabel);
    }

    public LocalBuilder getLocal(string name) {
        LocalBuilder r;
        foreach (var names in Enumerable.Reverse(m_locals)) {
            if (names.TryGetValue(name, out r)) return r;
        }
        return null;
    }
    public int getArg(string name) {
        int r;
        if (m_args.TryGetValue(name, out r)) return r;
        else return -1;
    }
    private Dictionary<string, int> m_args = new Dictionary<string,int>();
    private List<Dictionary<string, LocalBuilder>> m_locals = new List<Dictionary<string,LocalBuilder>>();
    private List<Label> m_breakLabels = new List<Label>();
    private List<Label> m_continueLabels = new List<Label>();
    private Label m_retLabel;
}

public class CodeEmitor {
    public CodeEmitor(string name, Dictionary<string, FuncMeta> funcs) {
        Name = name;
        m_fileName = name + "_Script.exe";
        FuncName2MethodBuilder = new Dictionary<string, MethodBuilder>();

        AsmBuilder = AppDomain.CurrentDomain.DefineDynamicAssembly(new AssemblyName(name), AssemblyBuilderAccess.RunAndSave);
        var moduleBuilder = AsmBuilder.DefineDynamicModule(name + "_MainModule", m_fileName);
        TypeBuilder = moduleBuilder.DefineType(name + "_MainClass");

        GlobalField = TypeBuilder.DefineField("s_global", typeof(Dictionary<string, Object>), FieldAttributes.Static | FieldAttributes.Private);
        buildStaticConstructor();

        var stmts = from kv in funcs select new StmtNodeVisitor_CodeEmitor(this, kv.Value);
        foreach (var stmt in stmts) stmt.emit();

        m_mainClass = TypeBuilder.CreateType();
    }
    public void run() {
        m_mainClass.InvokeMember("Main", BindingFlags.InvokeMethod, null, null, null);
    }
    public void save() {
        AsmBuilder.SetEntryPoint(m_mainClass.GetMethod("Main"), PEFileKinds.ConsoleApplication);
        AsmBuilder.Save(m_fileName);
    }
    private void buildStaticConstructor() {
        var methodBuilder = TypeBuilder.DefineConstructor(MethodAttributes.Static | MethodAttributes.Private, CallingConventions.Standard, null);
        var ilEmitor = methodBuilder.GetILGenerator();
        ilEmitor.Emit(OpCodes.Newobj, typeof(Dictionary<string, Object>).GetConstructor(new Type[]{}));
        ilEmitor.Emit(OpCodes.Stsfld, GlobalField);
        ilEmitor.Emit(OpCodes.Ret);
    }

    public string Name { get; private set; }
    public Dictionary<string, MethodBuilder> FuncName2MethodBuilder { get; private set; }
    public AssemblyBuilder AsmBuilder { get; private set; }
    public TypeBuilder TypeBuilder { get; private set; }
    public FieldBuilder GlobalField { get; private set; }
    private Type m_mainClass;
    private string m_fileName;
}
