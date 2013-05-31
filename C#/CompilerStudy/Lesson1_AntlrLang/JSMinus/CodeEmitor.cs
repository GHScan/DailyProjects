
using System;
using System.Collections.Generic;
using System.Linq;

using System.Diagnostics;
using System.Reflection;
using System.Reflection.Emit;

public class ExprNodeVisitor_CodeEmitor : IExprNodeVisitor {
    public ExprNodeVisitor_CodeEmitor(StmtNodeVisitor_CodeEmitor stmt, Type targetType, ExprNode node) {
        Trace.Assert(targetType != null);
        m_stmt = stmt;
        m_targetType = targetType;
        node.acceptVisitor(this);
        toTargetType();
    }
    private void toTargetType() {
        Trace.Assert(m_currentType != null);
        if (m_currentType == m_targetType) return;
        if (m_targetType == typeof(object)) {
            if (m_currentType == typeof(int)) {
                m_stmt.ILGenerator.Emit(OpCodes.Conv_R8);
                m_stmt.ILGenerator.Emit(OpCodes.Box, typeof(double));
            } else if (m_currentType == typeof(double)) {
                m_stmt.ILGenerator.Emit(OpCodes.Box, typeof(double));
            } else {
                Trace.Assert(false);
            }
        } else if (m_targetType == typeof(int)) {
            if (m_currentType == typeof(object)) {
                m_stmt.ILGenerator.Emit(OpCodes.Unbox_Any, typeof(double));
            }
            m_stmt.ILGenerator.Emit(OpCodes.Conv_I4);
        } else if (m_targetType == typeof(double)) {
            if (m_currentType == typeof(int)) {
                m_stmt.ILGenerator.Emit(OpCodes.Conv_R8);
            } else if (m_currentType == typeof(object)) {
                m_stmt.ILGenerator.Emit(OpCodes.Unbox_Any, typeof(double));
            } else {
                Trace.Assert(false);
            }
        } else {
            Trace.Assert(false);
        }
        m_currentType = m_targetType;
    }
    public void visit(ExprNode_ConstNumber node) {
        m_stmt.ILGenerator.Emit(OpCodes.Ldc_R8, node.Number);
        m_currentType = typeof(double);
    }
    public void visit(ExprNode_ConstString node) {
        if (node.Str == null) m_stmt.ILGenerator.Emit(OpCodes.Ldnull);
        else m_stmt.ILGenerator.Emit(OpCodes.Ldstr, node.Str);
        m_currentType = typeof(object);
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
            m_currentType = typeof(object);
            return;
        }
        var local = m_stmt.getLocal(node.Name);
        if (local != null) {
            // optimize ????
            m_stmt.ILGenerator.Emit(OpCodes.Ldloc, local);
            m_currentType = typeof(object);
            return;
        }

        m_stmt.ILGenerator.Emit(OpCodes.Ldsfld, m_stmt.Emitor.GlobalField);
        m_stmt.ILGenerator.Emit(OpCodes.Ldstr, node.Name);
        m_stmt.ILGenerator.Emit(OpCodes.Callvirt, typeof(Dictionary<string, object>).GetMethod("get_Item"));
        m_currentType = typeof(object);
    }
    public void visit(ExprNode_ArrayConstructor node) {
        m_stmt.ILGenerator.Emit(OpCodes.Newobj, typeof(List<object>).GetConstructor(new Type[]{}));
        if (node.Exprs != null) {
            foreach (var expr in node.Exprs) {
                m_stmt.ILGenerator.Emit(OpCodes.Dup);
                new ExprNodeVisitor_CodeEmitor(m_stmt, typeof(object), expr);
                m_stmt.ILGenerator.Emit(OpCodes.Callvirt, typeof(List<object>).GetMethod("Add"));
            }
        }
        m_currentType = typeof(object);
    }
    public void visit(ExprNode_UnaryOp node) {
        switch (node.Op) { 
            case "not":
                new ExprNodeVisitor_CodeEmitor(m_stmt, typeof(int), node.Expr);
                m_stmt.ILGenerator.Emit(OpCodes.Ldc_I4_0);
                m_stmt.ILGenerator.Emit(OpCodes.Ceq);
                m_currentType = typeof(int);
                break;
            case "-":
                new ExprNodeVisitor_CodeEmitor(m_stmt, typeof(double), node.Expr);
                m_stmt.ILGenerator.Emit(OpCodes.Neg);
                m_currentType = typeof(double);
                break;
            case "#":
                new ExprNodeVisitor_CodeEmitor(m_stmt, typeof(object), node.Expr);
                m_stmt.ILGenerator.Emit(OpCodes.Callvirt, typeof(List<object>).GetMethod("get_Count"));
                m_currentType = typeof(int);
                break;
            default: Trace.Assert(false);  break;
        }
    }
    public void visit(ExprNode_BinaryOp node) {
        if (node.Op == "&&" || node.Op == "||") {
            var endLabel = m_stmt.ILGenerator.DefineLabel();
            new ExprNodeVisitor_CodeEmitor(m_stmt, typeof(int), node.Lexpr);
            m_stmt.ILGenerator.Emit(OpCodes.Dup);
            if (node.Op == "&&") m_stmt.ILGenerator.Emit(OpCodes.Brfalse, endLabel);
            else m_stmt.ILGenerator.Emit(OpCodes.Brtrue, endLabel);
            m_stmt.ILGenerator.Emit(OpCodes.Pop);
            new ExprNodeVisitor_CodeEmitor(m_stmt, typeof(int), node.Rexpr);
            m_stmt.ILGenerator.MarkLabel(endLabel);
            m_currentType = typeof(int);
            return;
        }

        if (node.Op == "==" || node.Op == "!=") {
            new ExprNodeVisitor_CodeEmitor(m_stmt, typeof(object), node.Lexpr);
            new ExprNodeVisitor_CodeEmitor(m_stmt, typeof(object), node.Rexpr);
            m_stmt.ILGenerator.Emit(OpCodes.Call, typeof(object).GetMethod("Equals", new Type[] { typeof(object), typeof(object) }));
            if (node.Op == "!=") {
                m_stmt.ILGenerator.Emit(OpCodes.Ldc_I4_0);
                m_stmt.ILGenerator.Emit(OpCodes.Ceq);
            }
            m_currentType = typeof(int);
            return;
        }

        new ExprNodeVisitor_CodeEmitor(m_stmt, typeof(double), node.Lexpr);
        new ExprNodeVisitor_CodeEmitor(m_stmt, typeof(double), node.Rexpr);
        switch (node.Op) { 
            case "+":
                m_stmt.ILGenerator.Emit(OpCodes.Add);
                m_currentType = typeof(double);
                break;
            case "-":
                m_stmt.ILGenerator.Emit(OpCodes.Sub);
                m_currentType = typeof(double);
                break;
            case "*":
                m_stmt.ILGenerator.Emit(OpCodes.Mul);
                m_currentType = typeof(double);
                break;
            case "/":
                m_stmt.ILGenerator.Emit(OpCodes.Div);
                m_currentType = typeof(double);
                break;
            case "%":
                m_stmt.ILGenerator.Emit(OpCodes.Rem);
                m_currentType = typeof(double);
                break;
            case "<":
                m_stmt.ILGenerator.Emit(OpCodes.Clt);
                m_currentType = typeof(int);
                break;
            case "<=":
                m_stmt.ILGenerator.Emit(OpCodes.Cgt);
                m_stmt.ILGenerator.Emit(OpCodes.Ldc_I4_0);
                m_stmt.ILGenerator.Emit(OpCodes.Ceq);
                m_currentType = typeof(int);
                break;
            case ">":
                m_stmt.ILGenerator.Emit(OpCodes.Cgt);
                m_currentType = typeof(int);
                break;
            case ">=":
                m_stmt.ILGenerator.Emit(OpCodes.Clt);
                m_stmt.ILGenerator.Emit(OpCodes.Ldc_I4_0);
                m_stmt.ILGenerator.Emit(OpCodes.Ceq);
                m_currentType = typeof(int);
                break;
            default: Trace.Assert(false); break;
        }
    }
    public void visit(ExprNode_IndexOf node) {
        new ExprNodeVisitor_CodeEmitor(m_stmt, typeof(object), node.ArrayExpr);
        new ExprNodeVisitor_CodeEmitor(m_stmt, typeof(int), node.IndexExpr);
        m_stmt.ILGenerator.Emit(OpCodes.Callvirt, typeof(List<object>).GetMethod("get_Item"));
        m_currentType = typeof(object);
    }
    public void visit(ExprNode_Call node) {
        var funcName = (node.FuncExpr as ExprNode_ID).Name;
        if (node.ParamExprs != null) {
            foreach (var expr in node.ParamExprs) {
                new ExprNodeVisitor_CodeEmitor(m_stmt, typeof(object), expr);
            }
        }
        m_stmt.ILGenerator.Emit(OpCodes.Call, m_stmt.Emitor.FuncName2MethodInfo[funcName]);
        m_currentType = typeof(object);
    }
    private StmtNodeVisitor_CodeEmitor m_stmt;
    private Type m_targetType;
    private Type m_currentType;
}

public class StmtNodeVisitor_CodeEmitor : IStmtNodeVisitor {
    public StmtNodeVisitor_CodeEmitor(CodeEmitor emitor, FuncMeta meta) {
        Emitor = emitor;
        FuncMeta = meta;
        var methodBuilder = emitor.TypeBuilder.DefineMethod(meta.Name, MethodAttributes.Static | MethodAttributes.Public, CallingConventions.Standard,
            meta.Name == "Main" ? typeof(int) : typeof(object), Enumerable.Repeat(typeof(object), meta.ArgCount).ToArray());
        ILGenerator = methodBuilder.GetILGenerator();
        emitor.FuncName2MethodInfo[meta.Name] = methodBuilder;
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
        var local = allocLocal();
        m_locals[m_locals.Count - 1][node.Name] = local;
    }
    public void visit(StmtNode_DeclareArg node) {
        Trace.Assert(!m_args.ContainsKey(node.Name));
        m_args[node.Name] = m_args.Count;
    }
    public void visit(StmtNode_Block node) {
        m_locals.Add(new Dictionary<string, LocalBuilder>());
        foreach (var stmt in node.Stmts) stmt.acceptVisitor(this);
        foreach (var kv in m_locals[m_locals.Count - 1]) {
            freeLocal(kv.Value);
        }
        m_locals.RemoveAt(m_locals.Count - 1);
    }
    public void visit(StmtNode_Stmts node) {
        foreach (var stmt in node.Stmts) stmt.acceptVisitor(this);
    }
    public void visit(StmtNode_Assign node) {
        var indexOfExpr = node.Lexpr as ExprNode_IndexOf;
        if (indexOfExpr != null) {
            new ExprNodeVisitor_CodeEmitor(this, typeof(object), indexOfExpr.ArrayExpr);
            new ExprNodeVisitor_CodeEmitor(this, typeof(int), indexOfExpr.IndexExpr);
            new ExprNodeVisitor_CodeEmitor(this, typeof(object), node.Rexpr);
            ILGenerator.Emit(OpCodes.Callvirt, typeof(List<object>).GetMethod("set_Item"));
        } else {
            var idExpr = node.Lexpr as ExprNode_ID;
            Trace.Assert(idExpr != null);

            var argIdx = getArg(idExpr.Name);
            if (argIdx != -1) {
                new ExprNodeVisitor_CodeEmitor(this, typeof(object), node.Rexpr);
                if (argIdx < 256) ILGenerator.Emit(OpCodes.Starg_S, argIdx);
                else ILGenerator.Emit(OpCodes.Starg, argIdx);
                return;
            }
            var local = getLocal(idExpr.Name);
            if (local != null) {
                new ExprNodeVisitor_CodeEmitor(this, typeof(object), node.Rexpr);
                ILGenerator.Emit(OpCodes.Stloc, local);
                return;
            }

            ILGenerator.Emit(OpCodes.Ldsfld, Emitor.GlobalField);
            ILGenerator.Emit(OpCodes.Ldstr, idExpr.Name);
            new ExprNodeVisitor_CodeEmitor(this, typeof(object), node.Rexpr);
            ILGenerator.Emit(OpCodes.Callvirt, typeof(Dictionary<string, object>).GetMethod("set_Item"));
        }
    }
    public void visit(StmtNode_Call node) {
        new ExprNodeVisitor_CodeEmitor(this, typeof(object), node.CallExpr);
        ILGenerator.Emit(OpCodes.Pop);
    }
    public void visit(StmtNode_IfElse node) {
        var elseLabel = ILGenerator.DefineLabel();
        var endLabel = ILGenerator.DefineLabel();
        new ExprNodeVisitor_CodeEmitor(this, typeof(int), node.Expr);
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
        new ExprNodeVisitor_CodeEmitor(this, typeof(int), node.Second);
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
            new ExprNodeVisitor_CodeEmitor(this, typeof(object), node.Expr);
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
    private LocalBuilder allocLocal() {
        if (m_localPool.Count == 0) m_localPool.Add(ILGenerator.DeclareLocal(typeof(object)));
        var r = m_localPool[m_localPool.Count - 1];
        m_localPool.RemoveAt(m_localPool.Count - 1);
        return r;
    }
    private void freeLocal(LocalBuilder l) {
        m_localPool.Add(l);
    }
    private Dictionary<string, int> m_args = new Dictionary<string,int>();
    private List<Dictionary<string, LocalBuilder>> m_locals = new List<Dictionary<string,LocalBuilder>>();
    private List<Label> m_breakLabels = new List<Label>();
    private List<Label> m_continueLabels = new List<Label>();
    private Label m_retLabel;
    private List<LocalBuilder> m_localPool = new List<LocalBuilder>();
}

public class CodeEmitor {
    public CodeEmitor(string name, Dictionary<string, FuncMeta> funcs) {
        Name = name;
        m_fileName = name + "_Script.exe";
        FuncName2MethodInfo = new Dictionary<string, MethodInfo>();

        AsmBuilder = AppDomain.CurrentDomain.DefineDynamicAssembly(new AssemblyName(name), AssemblyBuilderAccess.RunAndSave);
        var moduleBuilder = AsmBuilder.DefineDynamicModule(name + "_MainModule", m_fileName);
        TypeBuilder = moduleBuilder.DefineType(name + "_MainClass");

        GlobalField = TypeBuilder.DefineField("s_global", typeof(Dictionary<string, object>), FieldAttributes.Static | FieldAttributes.Private);
        buildStaticConstructor();
        BuildinCodeEmitor.emitBuildins(this);

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
        ilEmitor.Emit(OpCodes.Newobj, typeof(Dictionary<string, object>).GetConstructor(new Type[]{}));
        ilEmitor.Emit(OpCodes.Stsfld, GlobalField);

        ilEmitor.Emit(OpCodes.Ldsfld, GlobalField);
        ilEmitor.Emit(OpCodes.Ldstr, "__s_random");
        ilEmitor.Emit(OpCodes.Newobj, typeof(Random).GetConstructor(new Type[]{}));
        ilEmitor.Emit(OpCodes.Callvirt, typeof(Dictionary<string, object>).GetMethod("set_Item"));

        ilEmitor.Emit(OpCodes.Ret);
    }

    public string Name { get; private set; }
    public Dictionary<string, MethodInfo> FuncName2MethodInfo { get; private set; }
    public AssemblyBuilder AsmBuilder { get; private set; }
    public TypeBuilder TypeBuilder { get; private set; }
    public FieldBuilder GlobalField { get; private set; }
    private Type m_mainClass;
    private string m_fileName;
}
