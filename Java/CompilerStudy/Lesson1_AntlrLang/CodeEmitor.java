import java.util.*;
import java.io.*;
import org.objectweb.asm.*;
import org.objectweb.asm.commons.*;

class ExprNodeVisitor_CodeEmitor implements IExprNodeVisitor {
    public ExprNodeVisitor_CodeEmitor(StmtNodeVisitor_CodeEmitor stmt, Class targetType, ExprNode node) {
        m_stmt = stmt;
        node.acceptVisitor(this);
        castToTargetType();
    }

    private void castToTargetType() {
    }

    private StmtNodeVisitor_CodeEmitor m_stmt;
    private Class m_targetType;
    private Class m_currentType;

    @Override
    public void visit(ExprNode_ConstNumber node) {
    }
    @Override
    public void visit(ExprNode_ConstString node) {
    }
    @Override
    public void visit(ExprNode_ID node) {
    }
    @Override
    public void visit(ExprNode_ArrayConstructor node) {
    }
    @Override
    public void visit(ExprNode_UnaryOp node) {
    }
    @Override
    public void visit(ExprNode_BinaryOp node) {
    }
    @Override
    public void visit(ExprNode_IndexOf node) {
    }
    @Override
    public void visit(ExprNode_Call node) {
    }
}
class StmtNodeVisitor_CodeEmitor implements IStmtNodeVisitor, Opcodes {
    public StmtNodeVisitor_CodeEmitor(ClassInjector _ci, FuncMeta meta) {
        ci = _ci;

        Type[] paramsType = new Type[meta.argCount];
        Arrays.fill(paramsType, Type.getType(Object.class));
        mbuilder = new GeneratorAdapter(ACC_PUBLIC + ACC_STATIC, new Method(meta.name, Type.getType(Object.class), paramsType), null, null, ci);

        meta.stmt.acceptVisitor(this);

        //mbuilder.getStatic(Type.getType(System.class), "out", Type.getType(System.out.getClass()));
        //mbuilder.push("hello world");
        //mbuilder.invokeVirtual(Type.getType(System.out.getClass()), Method.getMethod("void println(String)"));

        mbuilder.push((String)null);
        mbuilder.returnValue();
        mbuilder.endMethod();
    }

    public ClassInjector ci;
    public GeneratorAdapter mbuilder;

    @Override
    public void visit(StmtNode_DeclareLocal node) {
    }
    @Override
    public void visit(StmtNode_DeclareArg node) {
    }
    @Override
    public void visit(StmtNode_Block node) {
    }
    @Override
    public void visit(StmtNode_Stmts node) {
    }
    @Override
    public void visit(StmtNode_Assign node) {
    }
    @Override
    public void visit(StmtNode_Call node) {
    }
    @Override
    public void visit(StmtNode_IfElse node) {
    }
    @Override
    public void visit(StmtNode_For node) {
    }
    @Override
    public void visit(StmtNode_Continue node) {
    }
    @Override
    public void visit(StmtNode_Break node) {
    }
    @Override
    public void visit(StmtNode_Return node) {
    }
}

class ClassInjector extends ClassVisitor {
    public ClassInjector(ClassVisitor cv, HashMap<String, FuncMeta> funcs) {
        super(Opcodes.ASM4, cv);
        globalName = "s_globals";
        m_funcs = funcs;
    }

    private HashMap<String, FuncMeta> m_funcs;
    private void injectMethods() {
        for (Map.Entry<String, FuncMeta> entry : m_funcs.entrySet()) {
            assert !func2Type.containsKey(entry.getKey());
            Type[] paramsType = new Type[entry.getValue().argCount];
            Arrays.fill(paramsType, Type.getType(Object.class));
            func2Type.put(entry.getKey(), Type.getMethodType(Type.getType(Object.class), paramsType));
        }

        for (Map.Entry<String, FuncMeta> entry : m_funcs.entrySet()) { 
            new StmtNodeVisitor_CodeEmitor(this, entry.getValue());
        }
        //System.out.println("class name:" + className);
        //System.out.println("class type:" + classType);
        //System.out.println("global name:" + globalName);
        //System.out.println("global type:" + globalType);
        //for (Map.Entry<String, Type> entry : func2Type.entrySet()) {
        //    System.out.println("func, type:" + entry.getKey() + "," + entry.getValue());
        //}
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
        func2Type.put(name, Type.getType(desc));
        return super.visitMethod(access, name, desc, signature, exceptions);
    }
    @Override
    public void visitEnd() {
        assert className != null;
        assert classType != null;
        assert globalType != null;
        assert func2Type.size() > 0;

        injectMethods();

        super.visitEnd();
    }

    public Type classType;
    public String className;
    public Type globalType; 
    public String globalName;
    public HashMap<String, Type> func2Type = new HashMap<String, Type>();
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
        Class dyclass = this.defineClass(m_name, m_codes, 0, m_codes.length);
        dyclass.getMethod("main", new Class[]{}).invoke(null, new Object[]{});
    }
    public void save() throws FileNotFoundException, IOException {
        FileOutputStream fos = new FileOutputStream(m_name + ".class");
        fos.write(m_codes);
        fos.close();
    }
    private byte[] m_codes;
    private String m_name;
}
