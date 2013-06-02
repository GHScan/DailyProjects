import java.util.*;
import java.io.*;
import org.objectweb.asm.*;
import org.objectweb.asm.commons.*;

class ExprNodeVisitor_CodeEmitor implements IExprNodeVisitor {
    public ExprNodeVisitor_CodeEmitor(StmtNodeVisitor_CodeEmitor stmt, Class<?> targetType, ExprNode node) {
        m_stmt = stmt;
        m_targetType = targetType;
        node.acceptVisitor(this);
        castToTargetType();
    }

    private void castToTargetType() {
        assert m_currentType != null;
        if (m_currentType == m_targetType) return;

        if (m_targetType == double.class) {
            if (m_currentType == int.class) {
                m_stmt.mbuilder.cast(Type.getType(int.class), Type.getType(double.class));
            } else if (m_currentType == Object.class) {
                m_stmt.mbuilder.unbox(Type.getType(double.class));
            } else {
                assert false;
            }
        } else if (m_targetType == int.class) {
            if (m_currentType == Object.class) {
                m_stmt.mbuilder.unbox(Type.getType(double.class));
            } 
            m_stmt.mbuilder.cast(Type.getType(double.class), Type.getType(int.class));
        } else if (m_targetType == Object.class) {
            if (m_currentType == int.class) {
                m_stmt.mbuilder.cast(Type.getType(int.class), Type.getType(double.class));
                m_stmt.mbuilder.box(Type.getType(double.class));
            } else if (m_currentType == double.class) {
                m_stmt.mbuilder.box(Type.getType(double.class));
            } else {
                assert false;
            }
        } else {
            assert false;
        }
        m_currentType = m_targetType;
    }

    private StmtNodeVisitor_CodeEmitor m_stmt;
    private Class<?> m_targetType;
    private Class<?> m_currentType;

    @Override
    public void visit(ExprNode_ConstNumber node) {
        if (m_targetType == int.class) {
            m_stmt.mbuilder.push((int)node.number);
            m_currentType = int.class;
        } else {
            m_stmt.mbuilder.push(node.number);
            m_currentType = double.class;
        }
    }
    @Override
    public void visit(ExprNode_ConstString node) {
        m_stmt.mbuilder.push(node.str);
        m_currentType = Object.class;
    }
    @Override
    public void visit(ExprNode_ID node) {
        m_currentType = Object.class;
        int localIdx = m_stmt.getLocalIndex(node.name);
        if (localIdx != -1) {
            m_stmt.mbuilder.loadLocal(localIdx);
            return;
        }
        int argIdx = m_stmt.getArgIndex(node.name);
        if (argIdx != -1) {
            m_stmt.mbuilder.loadArg(argIdx);
            return;
        }

        m_stmt.ci.emit_pushGlobal(m_stmt.mbuilder);
        m_stmt.mbuilder.push(node.name);
        m_stmt.ci.emit_getGlobal(m_stmt.mbuilder);
    }
    @Override
    public void visit(ExprNode_ArrayConstructor node) {
        m_stmt.mbuilder.newInstance(Type.getType(ArrayList.class));
        m_stmt.mbuilder.dup();
        m_stmt.mbuilder.invokeConstructor(Type.getType(ArrayList.class), Method.getMethod("void <init>()"));
        if (node.exprs != null) {
            for (ExprNode expr : node.exprs) {
                m_stmt.mbuilder.dup();
                new ExprNodeVisitor_CodeEmitor(m_stmt, Object.class, expr);
                m_stmt.mbuilder.invokeVirtual(Type.getType(ArrayList.class), Method.getMethod("boolean add(Object)"));
                m_stmt.mbuilder.pop();
            }
        }
        m_currentType = Object.class;
    }
    @Override
    public void visit(ExprNode_UnaryOp node) {
        switch (node.op) {
            case "not":
                new ExprNodeVisitor_CodeEmitor(m_stmt, int.class, node.expr);
                m_currentType = Object.class;
                break;
            case "-":
                new ExprNodeVisitor_CodeEmitor(m_stmt, double.class, node.expr);
                m_stmt.mbuilder.math(GeneratorAdapter.NEG, Type.getType(double.class));
                m_currentType = double.class;
                break;
            case "#":
                new ExprNodeVisitor_CodeEmitor(m_stmt, Object.class, node.expr);
                m_stmt.mbuilder.checkCast(Type.getType(ArrayList.class));
                m_stmt.mbuilder.invokeVirtual(Type.getType(ArrayList.class), Method.getMethod("int size()"));
                m_currentType = int.class;
                break;
            default:
                assert false;
                break;
        }
    }
    @Override
    public void visit(ExprNode_BinaryOp node) {
        if (node.op.equals("&&") || node.op.equals("||")) {
            Label endLabel = m_stmt.mbuilder.newLabel();
            new ExprNodeVisitor_CodeEmitor(m_stmt, int.class, node.lexpr);
            m_stmt.mbuilder.dup();
            if (node.op.equals("&&")) {
                m_stmt.mbuilder.ifZCmp(GeneratorAdapter.EQ, endLabel);
            } else {
                m_stmt.mbuilder.ifZCmp(GeneratorAdapter.NE, endLabel);
            }
            m_stmt.mbuilder.pop();
            new ExprNodeVisitor_CodeEmitor(m_stmt, int.class, node.rexpr);
            m_stmt.mbuilder.mark(endLabel);
            m_currentType = int.class;
            return;
        } 

        if (node.op.equals("==") || node.op.equals("!=")) {
            new ExprNodeVisitor_CodeEmitor(m_stmt, Object.class, node.lexpr);
            new ExprNodeVisitor_CodeEmitor(m_stmt, Object.class, node.rexpr);
            if (node.op.equals("==")) {
                m_stmt.mbuilder.invokeStatic(m_stmt.ci.classType, m_stmt.ci.funcName2Method.get("__equals"));
            } else {
                m_stmt.mbuilder.invokeStatic(m_stmt.ci.classType, m_stmt.ci.funcName2Method.get("__nequals"));
            }
            m_currentType = int.class;
            return;
        }

        new ExprNodeVisitor_CodeEmitor(m_stmt, double.class, node.lexpr);
        new ExprNodeVisitor_CodeEmitor(m_stmt, double.class, node.rexpr);
        switch (node.op) {
            case "+":
                m_stmt.mbuilder.math(GeneratorAdapter.ADD, Type.getType(double.class));
                m_currentType = double.class;
                break;
            case "-":
                m_stmt.mbuilder.math(GeneratorAdapter.SUB, Type.getType(double.class));
                m_currentType = double.class;
                break;
            case "*":
                m_stmt.mbuilder.math(GeneratorAdapter.MUL, Type.getType(double.class));
                m_currentType = double.class;
                break;
            case "/":
                m_stmt.mbuilder.math(GeneratorAdapter.DIV, Type.getType(double.class));
                m_currentType = double.class;
                break;
            case "%":
                m_stmt.mbuilder.math(GeneratorAdapter.REM, Type.getType(double.class));
                m_currentType = double.class;
                break;
            case "<":
            case "<=":
            case ">":
            case ">=":
                {
                    Label trueLabel = m_stmt.mbuilder.newLabel();
                    Label endLabel = m_stmt.mbuilder.newLabel();
                    switch (node.op) {
                        case "<":
                            m_stmt.mbuilder.ifCmp(Type.getType(double.class), GeneratorAdapter.LT, trueLabel);
                            break;
                        case "<=":
                            m_stmt.mbuilder.ifCmp(Type.getType(double.class), GeneratorAdapter.LE, trueLabel);
                            break;
                        case ">":
                            m_stmt.mbuilder.ifCmp(Type.getType(double.class), GeneratorAdapter.GT, trueLabel);
                            break;
                        case ">=":
                            m_stmt.mbuilder.ifCmp(Type.getType(double.class), GeneratorAdapter.GE, trueLabel);
                            break;
                    }
                    m_stmt.mbuilder.push(false);
                    m_stmt.mbuilder.goTo(endLabel);
                    m_stmt.mbuilder.mark(trueLabel);
                    m_stmt.mbuilder.push(true);
                    m_stmt.mbuilder.mark(endLabel);
                }
                m_currentType = int.class;
                break;
            default:
                assert false : String.format("line:%d,op:%s", node.line, node.op);
                break;
        }
    }
    @Override
    public void visit(ExprNode_IndexOf node) {
        m_currentType = Object.class;
        new ExprNodeVisitor_CodeEmitor(m_stmt, Object.class, node.arrayExpr);
        m_stmt.mbuilder.checkCast(Type.getType(ArrayList.class));
        new ExprNodeVisitor_CodeEmitor(m_stmt, int.class, node.indexExpr);
        m_stmt.mbuilder.invokeVirtual(Type.getType(ArrayList.class), Method.getMethod("Object get(int)"));
    }
    @Override
    public void visit(ExprNode_Call node) {
        m_currentType = Object.class;
        assert node.funcExpr instanceof ExprNode_ID;
        String funcName = ((ExprNode_ID)node.funcExpr).name;
        if (node.paramExprs != null) {
            for (ExprNode expr : node.paramExprs) {
                new ExprNodeVisitor_CodeEmitor(m_stmt, Object.class, expr);
            }
        }
        m_stmt.mbuilder.invokeStatic(m_stmt.ci.classType, m_stmt.ci.funcName2Method.get(funcName));
    }
}
class StmtNodeVisitor_CodeEmitor implements IStmtNodeVisitor, Opcodes {
    public StmtNodeVisitor_CodeEmitor(ClassInjector _ci, FuncMeta meta) {
        ci = _ci;

        Type[] paramsType = new Type[meta.argCount];
        Arrays.fill(paramsType, Type.getType(Object.class));
        mbuilder = new GeneratorAdapter(ACC_PUBLIC + ACC_STATIC, new Method(meta.name, Type.getType(Object.class), paramsType), null, null, ci);

        m_retLabel = mbuilder.newLabel();

        meta.stmt.acceptVisitor(this);

        mbuilder.push((String)null);
        mbuilder.mark(m_retLabel);
        mbuilder.returnValue();
        mbuilder.endMethod();
    }

    public ClassInjector ci;
    public GeneratorAdapter mbuilder;

    public int getArgIndex(String name) {
        if (m_args.containsKey(name)) return (int)m_args.get(name);
        return -1;
    }
    public int getLocalIndex(String name) {
        for (int i = m_locals.size() - 1; i >= 0; --i) {
            if (m_locals.get(i).containsKey(name)) return (int)m_locals.get(i).get(name);
        }
        return -1;
    }

    private int allocLocal() {
        if (m_freeLocals.size() == 0) m_freeLocals.add(mbuilder.newLocal(Type.getType(Object.class)));
        int r = m_freeLocals.get(m_freeLocals.size() - 1);
        m_freeLocals.remove(m_freeLocals.size() - 1);
        return r;
    }
    private void freeLocal(int localIdx) {
        m_freeLocals.add(localIdx);
    }

    private HashMap<String, Integer> m_args = new HashMap<String, Integer>();
    private ArrayList<HashMap<String, Integer>> m_locals = new ArrayList<HashMap<String, Integer>>();
    private ArrayList<Integer> m_freeLocals = new ArrayList<Integer>();
    private Label m_retLabel;
    private ArrayList<Label> m_continueLabels = new ArrayList<Label>(); 
    private ArrayList<Label> m_breakLabels = new ArrayList<Label>();

    @Override
    public void visit(StmtNode_DeclareLocal node) {
        assert !m_locals.get(m_locals.size() - 1).containsKey(node.name);
        m_locals.get(m_locals.size() - 1).put(node.name, allocLocal());
    }
    @Override
    public void visit(StmtNode_DeclareArg node) {
        assert !m_args.containsKey(node.name);
        m_args.put(node.name, m_args.size());
    }
    @Override
    public void visit(StmtNode_Block node) {
        m_locals.add(new HashMap<String, Integer>());
        for (StmtNode stmt : node.stmts) {
            stmt.acceptVisitor(this);
        }
        for (int localIdx : m_locals.get(m_locals.size() - 1).values()) {
            freeLocal(localIdx);
        }
        m_locals.remove(m_locals.size() - 1);
    }
    @Override
    public void visit(StmtNode_Stmts node) {
        for (StmtNode stmt : node.stmts) {
            stmt.acceptVisitor(this);
        }
    }
    @Override
    public void visit(StmtNode_Assign node) {
        if (node.lexpr instanceof ExprNode_ID) {
            ExprNode_ID idExpr = (ExprNode_ID)node.lexpr;
            int localIdx = getLocalIndex(idExpr.name);
            if (localIdx != -1) {
                new ExprNodeVisitor_CodeEmitor(this, Object.class, node.rexpr);
                mbuilder.storeLocal(localIdx);
                return;
            }
            int argIdx = getArgIndex(idExpr.name);
            if (argIdx != -1) {
                new ExprNodeVisitor_CodeEmitor(this, Object.class, node.rexpr);
                mbuilder.storeArg(argIdx);
                return;
            }

            ci.emit_pushGlobal(mbuilder);
            mbuilder.push(idExpr.name);
            new ExprNodeVisitor_CodeEmitor(this, Object.class, node.rexpr);
            ci.emit_setGlobal(mbuilder);
        } else if (node.lexpr instanceof ExprNode_IndexOf) {
            ExprNode_IndexOf indexExpr = (ExprNode_IndexOf)node.lexpr;
            new ExprNodeVisitor_CodeEmitor(this, Object.class, indexExpr.arrayExpr);
            mbuilder.checkCast(Type.getType(ArrayList.class));
            new ExprNodeVisitor_CodeEmitor(this, int.class, indexExpr.indexExpr);
            new ExprNodeVisitor_CodeEmitor(this, Object.class, node.rexpr);
            mbuilder.invokeVirtual(Type.getType(ArrayList.class), Method.getMethod("Object set(int, Object)"));
            mbuilder.pop();
        } else {
            assert false;
        }
    }
    @Override
    public void visit(StmtNode_Call node) {
        new ExprNodeVisitor_CodeEmitor(this, Object.class, node.callExpr);
        mbuilder.pop();
    }
    @Override
    public void visit(StmtNode_IfElse node) {
        Label trueLabel = mbuilder.newLabel();
        Label endLabel = mbuilder.newLabel();
        new ExprNodeVisitor_CodeEmitor(this, int.class, node.expr);
        mbuilder.ifZCmp(GeneratorAdapter.NE, trueLabel);
        if (node.elseStmt != null) node.elseStmt.acceptVisitor(this);
        mbuilder.goTo(endLabel);
        mbuilder.mark(trueLabel);
        if (node.ifStmt != null) node.ifStmt.acceptVisitor(this);
        mbuilder.mark(endLabel);
    }
    @Override
    public void visit(StmtNode_For node) {
        if (node.first != null) node.first.acceptVisitor(this);

        Label loopLabel = mbuilder.newLabel();
        m_breakLabels.add(mbuilder.newLabel());
        m_continueLabels.add(mbuilder.newLabel());

        mbuilder.mark(loopLabel);
        new ExprNodeVisitor_CodeEmitor(this, int.class, node.second);
        mbuilder.ifZCmp(GeneratorAdapter.EQ, m_breakLabels.get(m_breakLabels.size() - 1));
        if (node.body != null) node.body.acceptVisitor(this);
        mbuilder.mark(m_continueLabels.get(m_continueLabels.size() - 1));
        if (node.third != null) node.third.acceptVisitor(this);
        mbuilder.goTo(loopLabel);
        mbuilder.mark(m_breakLabels.get(m_breakLabels.size() - 1));

        m_breakLabels.remove(m_breakLabels.size() - 1);
        m_continueLabels.remove(m_continueLabels.size() - 1);
    }
    @Override
    public void visit(StmtNode_Continue node) {
        mbuilder.goTo(m_continueLabels.get(m_continueLabels.size() - 1));
    }
    @Override
    public void visit(StmtNode_Break node) {
        mbuilder.goTo(m_breakLabels.get(m_breakLabels.size() - 1));
    }
    @Override
    public void visit(StmtNode_Return node) {
        if (node.expr != null) new ExprNodeVisitor_CodeEmitor(this, Object.class, node.expr);
        else mbuilder.push((String)null);
        mbuilder.goTo(m_retLabel);
    }
}

class ClassInjector extends ClassVisitor {
    public ClassInjector(ClassVisitor cv, HashMap<String, FuncMeta> funcs) {
        super(Opcodes.ASM4, cv);
        globalName = "s_globals";
        m_funcs = funcs;
    }

    public void emit_pushGlobal(GeneratorAdapter mbuilder) {
        mbuilder.getStatic(classType, globalName, globalType);
    }
    public void emit_getGlobal(GeneratorAdapter mbuilder) {
        mbuilder.invokeVirtual(globalType, Method.getMethod("Object get(Object)"));
    }
    public void emit_setGlobal(GeneratorAdapter mbuilder) {
        mbuilder.invokeVirtual(globalType, Method.getMethod("Object put(Object, Object)"));
        mbuilder.pop();
    }

    private HashMap<String, FuncMeta> m_funcs;
    private void injectMethods() {
        for (Map.Entry<String, FuncMeta> entry : m_funcs.entrySet()) {
            assert !funcName2Method.containsKey(entry.getKey());
            Type[] paramsType = new Type[entry.getValue().argCount];
            Arrays.fill(paramsType, Type.getType(Object.class));
            funcName2Method.put(entry.getKey(), new Method(entry.getKey(), Type.getType(Object.class), paramsType));
        }

        for (Map.Entry<String, FuncMeta> entry : m_funcs.entrySet()) { 
            new StmtNodeVisitor_CodeEmitor(this, entry.getValue());
        }
    }

    @Override
    public void visit(int version, int access, String name, String signature, String superName, String[] interfaces) {
        className = name;
        classType = Type.getObjectType(className);
        super.visit(version, access, name, signature, superName, interfaces);
    }
    @Override
    public FieldVisitor visitField(int access, String name, String desc, String signature, Object value) {
        if (name.equals(globalName)) {
            globalType = Type.getType(desc);
        }
        return super.visitField(access, name, desc, signature, value);
    }
    @Override
    public MethodVisitor visitMethod(int access, String name, String desc, String signature, String[] exceptions) {
        funcName2Method.put(name, new Method(name, desc));
        return super.visitMethod(access, name, desc, signature, exceptions);
    }
    @Override
    public void visitEnd() {
        assert className != null;
        assert classType != null;
        assert globalType != null;
        assert funcName2Method.size() > 0;

        injectMethods();

        super.visitEnd();
    }

    public Type classType;
    public String className;
    public Type globalType; 
    public String globalName;
    public HashMap<String, Method> funcName2Method = new HashMap<String, Method>();
}

class CodeEmitor extends ClassLoader implements Opcodes {
    public CodeEmitor(HashMap<String, FuncMeta> funcs) throws IOException {
        ClassReader cr = new ClassReader(new FileInputStream("template/JSMinusScript.class"));
        ClassWriter cw = new ClassWriter(cr, ClassWriter.COMPUTE_FRAMES);
        ClassInjector ci = new ClassInjector(cw, funcs);
        cr.accept(ci, 0);
        m_codes = cw.toByteArray();
        m_name = ci.className;
    }
    public void run() throws NoSuchMethodException, IllegalAccessException, java.lang.reflect.InvocationTargetException {
        Class<?> dyclass = this.defineClass(m_name, m_codes, 0, m_codes.length);
        dyclass.getMethod("__main", new Class<?>[]{}).invoke(null, new Object[]{});
    }
    public void save() throws FileNotFoundException, IOException {
        FileOutputStream fos = new FileOutputStream(m_name + ".class");
        fos.write(m_codes);
        fos.close();
    }
    private byte[] m_codes;
    private String m_name;
}
