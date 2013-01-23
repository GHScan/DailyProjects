
#include "pch.h"
#include "ByteCode.h"

class ConstantTable
{
public:
    static ConstantTable* instance()
    {
        static ConstantTable s_ins;
        return &s_ins;
    }
    int add(const Value& v)
    {
        auto iter = m_value2ID.lower_bound(v);
        if (iter != m_value2ID.end() && iter->first == v) {
            return iter->second;
        }
        int ID = (int)m_ID2Value.size();
        m_value2ID.insert(iter, make_pair(v, ID));
        m_ID2Value.push_back(v);
        return ID;
    }
    const Value& get(int ID) const
    {
        return m_ID2Value[ID];
    }
private:
    ConstantTable(){}
private:
    vector<Value> m_ID2Value;
    map<Value, int> m_value2ID;
};
class SymbolTable
{
public:
    SymbolTable():m_localsTop(0){}
    void beginBlock()
    {
        m_blockNames.resize(m_blockNames.size() + 1);
    }
    void endBlock()
    {
        m_localsTop -= (int)m_blockNames.back().size();
        m_blockNames.pop_back();
    }
    void declareLocal(const string& name)
    {
        ASSERT(m_blockNames.back().count(name) == 0);
        m_blockNames.back()[name] = m_localsTop++;
    }
    int getLocal(const string& name)
    {
        int r = -1;
        for (int i = m_blockNames.size() - 1; i >= 0; --i) {
            if (m_blockNames[i].count(name)) {
                r = m_blockNames[i][name];
                break;
            }
        }
        ASSERT(r != -1);
        return r;
    }
private:
    vector<map<string, int> > m_blockNames;
    int m_localsTop;
};
////////// ByteCode
enum EByteCode
{
    EBC_ClearEvalStack,
    EBC_Jump,
    EBC_NJump,
    EBC_SetReturn,

    EBC_WriteLocal,
    EBC_ReadLocal,
    EBC_ReadConstant,
    EBC_ArithmeticOp, // +,-,*,/,%
    EBC_RelationOp, // <,>,<=,>=,==,!=
    EBC_Not,
    EBC_Call,

    EBC_Push,
    EBC_Pop,

    EBC_MAX,
};
static_assert(EBC_MAX < 256, "EByteCode should be packed in a byte.");

template<int n>
struct ByteCodeOp;

template<>
struct ByteCodeOp<EBC_ClearEvalStack>
{
    static int emit(SymbolTable *symTable)
    {
        return (EBC_ClearEvalStack << 24) | 0;
    }
    static void execute(int code, StackFrame *frame)
    {
        frame->evalStack.clear();
    }
    static void disassembly(int code, ostream& so)
    {
        so << format("ClearLocal") << endl;
    }
};
template<>
struct ByteCodeOp<EBC_Jump>
{
    static int emit(SymbolTable *symTable, int ip)
    {
        ASSERT(ip < (1 << 24));
        return (EBC_Jump << 24) | ip;
    }
    static void execute(int code, StackFrame *frame)
    {
        int ip = code & 0xffffff;
        frame->ip = ip;
    }
    static void disassembly(int code, ostream& so)
    {
        int ip = code & 0xffffff;
        so << format("Jump %d", ip) << endl;
    }
};
template<>
struct ByteCodeOp<EBC_NJump>
{
    static int emit(SymbolTable *symTable, int ip)
    {
        ASSERT(ip < (1 << 24));
        return (EBC_NJump << 24) | ip;
    }
    static void execute(int code, StackFrame *frame)
    {
        int ip = code & 0xffffff;
        auto& evalStack = frame->evalStack;
        bool b = evalStack.back().toBoolean();
        evalStack.pop_back();
        if (b) frame->ip = ip;
    }
    static void disassembly(int code, ostream& so)
    {
        int ip = code & 0xffffff;
        so << format("NJump %d", ip) << endl;
    }
};
template<>
struct ByteCodeOp<EBC_SetReturn>
{
    static int emit(SymbolTable *symTable)
    {
        return (EBC_SetReturn << 24);
    }
    static void execute(int code, StackFrame *frame)
    {
        frame->retValue = frame->evalStack.back();
        frame->evalStack.pop_back();
    }
    static void disassembly(int code, ostream& so)
    {
        so << format("SetReturn") << endl;
    }
};
template<>
struct ByteCodeOp<EBC_WriteLocal>
{
    static int emit(SymbolTable *symTable, const string& name)
    {
        int ID = symTable->getLocal(name);
        ASSERT(ID < (1 << 24));
        return (EBC_WriteLocal << 24) | ID;
    }
    static void execute(int code, StackFrame *frame)
    {
        int ID = code & 0xffffff;
        auto &evalStack = frame->evalStack;
        frame->local(ID) = std::move(evalStack.back());
        evalStack.pop_back();
    }
    static void disassembly(int code, ostream& so)
    {
        int ID = code & 0xffffff;
        so << format("WriteLocal %d", ID) << endl;
    }
};
template<>
struct ByteCodeOp<EBC_ReadLocal>
{
    static int emit(SymbolTable *symTable, const string& name)
    {
        int ID = symTable->getLocal(name);
        ASSERT(ID < (1 << 24));
        return (EBC_ReadLocal << 24) | ID;
    }
    static void execute(int code, StackFrame *frame)
    {
        int ID = code & 0xffffff;
        auto &evalStack = frame->evalStack;
        evalStack.push_back(frame->local(ID));
    }
    static void disassembly(int code, ostream& so)
    {
        int ID = code & 0xffffff;
        so << format("ReadLocal %d", ID) << endl;
    }
};
template<>
struct ByteCodeOp<EBC_ReadConstant>
{
    static int emit(SymbolTable *symTable, const Value& val)
    {
        int ID = ConstantTable::instance()->add(val);
        ASSERT(ID < (1 << 24));
        return (EBC_ReadConstant << 24) | ID;
    }
    static void execute(int code, StackFrame *frame)
    {
        int ID = code & 0xffffff;
        frame->evalStack.push_back(ConstantTable::instance()->get(ID));
    }
    static void disassembly(int code, ostream& so)
    {
        int ID = code & 0xffffff;
        so << format("ReadConstant %s", ConstantTable::instance()->get(ID).toString().c_str()) << endl;
    }
};
template<>
struct ByteCodeOp<EBC_ArithmeticOp>
{
    enum OpCode
    {
        OC_Add, OC_Sub, OC_Mul, OC_Div, OC_Mod,
    };
    static string op2string(int op)
    {
        switch (op) {
            case OC_Add: return "+";
            case OC_Sub: return "-";
            case OC_Mul: return "*";
            case OC_Div: return "/";
            case OC_Mod: return "%";
            default: ASSERT(0); 
        }
        return "";
    }
    static int string2op(const string& str)
    {
        if (str == "+") return OC_Add;
        else if (str == "-") return OC_Sub;
        else if (str == "*") return OC_Mul;
        else if (str == "/") return OC_Div;
        else if (str == "%") return OC_Mod;
        else ASSERT(0);
        return 0;
    }

    static int emit(SymbolTable *symTable, const string& op)
    {
        return (EBC_ArithmeticOp << 24) | string2op(op);
    }
    static void execute(int code, StackFrame *frame)
    {
        int op = code & 0xffffff;
        auto& evalStack = frame->evalStack;
        auto last = std::move(evalStack.back());
        evalStack.pop_back();
        switch (op) {
            case OC_Add: evalStack.back() += last; break;
            case OC_Sub: evalStack.back() -= last; break;
            case OC_Mul: evalStack.back() *= last; break;
            case OC_Div: evalStack.back() /= last; break;
            case OC_Mod: evalStack.back() %= last; break;
            default: ASSERT(0); 
        }
    }
    static void disassembly(int code, ostream& so)
    {
        int op = code & 0xffffff;
        so << format("Arithmetic %s", op2string(op).c_str()) << endl;
    }
};
template<>
struct ByteCodeOp<EBC_RelationOp>
{
    enum OpCode
    {
        OC_Equal, OC_NotEqual, OC_Less, OC_Greater, OC_LessEqual, OC_GreaterEqual,
    };
    static string op2string(int op)
    {
        switch (op) {
            case OC_Equal: return "==";
            case OC_NotEqual: return "!=";
            case OC_Less: return "<";
            case OC_Greater: return ">";
            case OC_LessEqual: return "<=";
            case OC_GreaterEqual: return ">=";
            default: ASSERT(0);
        }
        return "";
    }
    static int string2op(const string& str)
    {
        if (str == "==") return OC_Equal;
        else if (str == "!=") return OC_NotEqual;
        else if (str == "<") return OC_Less;
        else if (str == ">") return OC_Greater;
        else if (str == "<=") return OC_LessEqual;
        else if (str == ">=") return OC_GreaterEqual;
        else ASSERT(0);
        return 0;
    }

    static int emit(SymbolTable *symTable, const string& op)
    {
        return (EBC_RelationOp << 24) | string2op(op);
    }
    static void execute(int code, StackFrame *frame)
    {
        int op = code & 0xffffff;
        auto& evalStack = frame->evalStack;
        auto& left = evalStack[evalStack.size() - 2];
        auto& right = evalStack.back();
        bool b = false;
        switch (op) {
            case OC_Equal: b = left == right; break;
            case OC_NotEqual: b = left != right; break;
            case OC_Less: b = left < right; break;
            case OC_Greater: b = left > right; break;
            case OC_LessEqual: b = left <= right; break;
            case OC_GreaterEqual: b = left >= right; break;
            default: ASSERT(0); break;
        }
        evalStack.pop_back();
        evalStack.back() = Value::createBoolean(b);
    }
    static void disassembly(int code, ostream& so)
    {
        int op = code & 0xffffff;
        so << format("RelationOp %s", op2string(op).c_str()) << endl;
    }
};
template<>
struct ByteCodeOp<EBC_Not>
{
    static int emit(SymbolTable *symTable)
    {
        return (EBC_Not << 24);
    }
    static void execute(int code, StackFrame *frame)
    {
        frame->evalStack.back()._not();
    }
    static void disassembly(int code, ostream& so)
    {
        so << format("Not") << endl;
    }
};
template<>
struct ByteCodeOp<EBC_Call>
{
    static int emit(SymbolTable *symTable, const string& name, int argc)
    {
        int ID = ConstantTable::instance()->add(Value::createString(name));
        ASSERT(ID < (1 << 16));
        ASSERT(argc < (1 << 8));
        return (EBC_Call << 24) | (ID << 16) | argc;
    }
    static void execute(int code, StackFrame *frame)
    {
        int ID = (code >> 8) & 0xffff;
        int argc = code & 0xff;
        auto& func = GlobalEnvironment::instance()->getFunc(ConstantTable::instance()->get(ID).toString());
        auto& evalStack = frame->evalStack;
        vector<Value> args;
        for (auto iter = evalStack.end() - argc; iter != evalStack.end(); ++iter) {
            args.push_back(std::move(*iter));
        }
        evalStack.erase(evalStack.end() - argc, evalStack.end());
        Value r = func->call(args);
        evalStack.push_back(r);
    }
    static void disassembly(int code, ostream& so)
    {
        int ID = (code >> 8) & 0xffff;
        int argc = code & 0xff;
        so << format("Call %s, %d", ConstantTable::instance()->get(ID).toString().c_str(), argc) << endl;
    }
};

template<>
struct ByteCodeOp<EBC_Push>
{
    static int emit(SymbolTable *symTable, int off)
    {
        ASSERT(off < 0);
        off = -off;
        ASSERT(off < (1 << 24));
        return (EBC_Push << 24) | off;
    }
    static void execute(int code, StackFrame *frame)
    {
        int off = -(code & 0xffffff);
        auto& evalStack = frame->evalStack;
        evalStack.push_back(evalStack[evalStack.size() + off]);
    }
    static void disassembly(int code, ostream& so)
    {
        int off = -(code & 0xffffff);
        so << format("Push %d", off) << endl;
    }
};

template<>
struct ByteCodeOp<EBC_Pop>
{
    static int emit(SymbolTable *symTable, int n)
    {
        ASSERT(n < (1 << 24));
        return (EBC_Pop << 24) | n;
    }
    static void execute(int code, StackFrame *frame)
    {
        int n = code & 0xffffff;
        auto& evalStack = frame->evalStack;
        evalStack.erase(evalStack.end() - n, evalStack.end());
    }
    static void disassembly(int code, ostream& so)
    {
        int n = code & 0xffffff;
        so << format("Pop %d", n) << endl;
    }
};
//////////

#define EMIT(code, ...) m_codes.push_back(ByteCodeOp<code>::emit(m_symTable, __VA_ARGS__))
#define EMIT0(code) m_codes.push_back(ByteCodeOp<code>::emit(m_symTable))
#define PRE_EMIT(off) { off = (int)m_codes.size(); m_codes.push_back(0);}
#define POST_EMIT(off, code, ...) {m_codes[off] = ByteCodeOp<code>::emit(m_symTable, __VA_ARGS__);}
#define POST_EMIT0(off, code) {m_codes[off] = ByteCodeOp<code>::emit(m_symTable);}
class ExpNodeVisitor_CodeEmittor:
    public IExpNodeVisitor
{
public:
    ExpNodeVisitor_CodeEmittor(vector<int>& codes, SymbolTable* symTable, ExpNodePtr exp):
        m_codes(codes), m_symTable(symTable)
    {
        exp->acceptVisitor(this);
    }
private:
    virtual void visit(ExpNode_Assign *v) 
    {
        v->right->acceptVisitor(this);
        EMIT(EBC_Push, -1);
        EMIT(EBC_WriteLocal, v->name);
    }
    virtual void visit(ExpNode_BinaryOp *v) 
    {
        if (v->op == "+" || v->op == "-" || v->op == "*" || v->op == "/" || v->op == "%") {
            v->left->acceptVisitor(this);
            v->right->acceptVisitor(this);
            EMIT(EBC_ArithmeticOp, v->op);
        }
        else if (v->op == "==" || v->op == "!=" || v->op == "<" || v->op == ">" || v->op == "<=" || v->op == ">=")
        {
            v->left->acceptVisitor(this);
            v->right->acceptVisitor(this);
            EMIT(EBC_RelationOp, v->op);
        }
        else if (v->op == "&&" || v->op == "||") {
            if (v->op == "&&") {
                /*
                    exp1
                    push -1
                    njump l1
                    jump l2
l1:
                    pop 1
                    exp2
l2:
                 */
                int njump, jump;
                v->left->acceptVisitor(this);
                EMIT(EBC_Push, -1);
                PRE_EMIT(njump);
                PRE_EMIT(jump);
                POST_EMIT(njump, EBC_NJump, (int)m_codes.size());
                EMIT(EBC_Pop, 1);
                v->right->acceptVisitor(this);
                POST_EMIT(jump, EBC_Jump, (int)m_codes.size());
            }
            else {
                /*
                   exp1 
                   push -1
                   njump l1
                   pop 1
                   exp2
l1:
                 */
                int njump;
                v->left->acceptVisitor(this);
                EMIT(EBC_Push, -1);
                PRE_EMIT(njump);
                EMIT(EBC_Pop, 1);
                v->right->acceptVisitor(this);
                POST_EMIT(njump, EBC_NJump, (int)m_codes.size());
            }
        }
        else ASSERT(0);
    }
    virtual void visit(ExpNode_UnaryOp *v) 
    {
        if (v->op == "!") {
            v->left->acceptVisitor(this);
            EMIT0(EBC_Not);
        }
        else ASSERT(0);
    }
    virtual void visit(ExpNode_Constant *v) 
    {
        EMIT(EBC_ReadConstant, v->val);
    }
    virtual void visit(ExpNode_Variable *v) 
    {
        EMIT(EBC_ReadLocal, v->name);
    }
    virtual void visit(ExpNode_Call *v) 
    {
        for (auto &exp : v->params) {
            exp->acceptVisitor(this);
        }
        EMIT(EBC_Call, v->name, (int)v->params.size());
    }
private:
    vector<int> &m_codes;
    SymbolTable *m_symTable;
};
class StmtNodeVisitor_CodeEmittor:
    public IStmtNodeVisitor
{
public:
    StmtNodeVisitor_CodeEmittor(vector<int>& codes, SymbolTable* symTable, StmtNodePtr stmt):
        m_codes(codes), m_symTable(symTable)
    {
        stmt->acceptVisitor(this);

        for (auto off : m_retCodeOff) {
            POST_EMIT(off, EBC_Jump, (int)m_codes.size());
        }
        m_retCodeOff.clear();
    }
private:
    virtual void visit(StmtNode_IfElse *node) 
    {
        /*
         exp
         njump l1
         elseStmt
         jump l2
l1:
        ifstmt
l2:
         */
        int njump, jump;
        (ExpNodeVisitor_CodeEmittor(m_codes, m_symTable, node->condition));
        PRE_EMIT(njump);
        if (node->elseStmt) node->elseStmt->acceptVisitor(this);
        PRE_EMIT(jump);
        POST_EMIT(njump, EBC_NJump, (int)m_codes.size());
        if (node->ifStmt) node->ifStmt->acceptVisitor(this);
        POST_EMIT(jump, EBC_Jump, (int)m_codes.size());
    }
    virtual void visit(StmtNode_For *node) 
    {
        /*
            exp1
            clear
l1:
            exp2
            njump l2
            jump l_break
l2:
            stmt
l_continue:
            exp3
            clear
            jump l1
l_break: 
         */
        int jump_l1, njump_l2, jump_break;
        if (node->s1) {
            (ExpNodeVisitor_CodeEmittor(m_codes, m_symTable, node->s1));
            EMIT0(EBC_ClearEvalStack);
        }
        int l1 = (int)m_codes.size();
        if (node->s2) {
            ExpNodeVisitor_CodeEmittor(m_codes, m_symTable, node->s2);
        }
        else {
            EMIT(EBC_ReadConstant, Value::createInt(1));
        }
        PRE_EMIT(njump_l2);
        PRE_EMIT(jump_break);
        POST_EMIT(njump_l2, EBC_NJump, (int)m_codes.size());
        if (node->stmt) node->stmt->acceptVisitor(this);
        // l_continue
        {
            for (auto off : m_continueCodeOff) {
                POST_EMIT(off, EBC_Jump, (int)m_codes.size());
            }
            m_continueCodeOff.clear();
        }
        if (node->s3) {
            ExpNodeVisitor_CodeEmittor(m_codes, m_symTable, node->s3);
            EMIT0(EBC_ClearEvalStack);
        }
        EMIT(EBC_Jump, l1);
        // l_break
        {
            for (auto off : m_breakCodeOff) {
                POST_EMIT(off, EBC_Jump, (int)m_codes.size());
            }
            m_breakCodeOff.clear();
        }
        POST_EMIT(jump_break, EBC_Jump, (int)m_codes.size());
    }
    virtual void visit(StmtNode_Continue *node) 
    {
        int jump;
        PRE_EMIT(jump);
        m_continueCodeOff.push_back(jump);
    }
    virtual void visit(StmtNode_Break *node) 
    {
        int jump;
        PRE_EMIT(jump);
        m_breakCodeOff.push_back(jump);
    }
    virtual void visit(StmtNode_Return *node) 
    {
        if (node->left) ExpNodeVisitor_CodeEmittor(m_codes, m_symTable, node->left);
        else EMIT(EBC_ReadConstant, Value());
        EMIT0(EBC_SetReturn);
        int jump;
        PRE_EMIT(jump);
        m_retCodeOff.push_back(jump);
    }
    virtual void visit(StmtNode_Local *node) 
    {
        m_symTable->declareLocal(node->name);
    }
    virtual void visit(StmtNode_Block *node) 
    {
        m_symTable->beginBlock();
        for (auto &stmt : node->stmts) {
            stmt->acceptVisitor(this);
        }
        m_symTable->endBlock();
    }
    virtual void visit(StmtNode_Exp *node) 
    {
        ExpNodeVisitor_CodeEmittor(m_codes, m_symTable, node->exp);
        EMIT0(EBC_ClearEvalStack);
    }
private:
    vector<int> &m_codes;
    SymbolTable *m_symTable;
    vector<int> m_retCodeOff;
    vector<int> m_continueCodeOff;
    vector<int> m_breakCodeOff;
};

ByteCodeSeq::ByteCodeSeq(StmtNodePtr stmt)
{
    SymbolTable symTable;
    StmtNodeVisitor_CodeEmittor(m_codes, &symTable, stmt);
}
void ByteCodeSeq::disassembly(ostream& so)
{
    for (auto code : m_codes) {
        switch (code >> 24) {
            case EBC_ClearEvalStack: ByteCodeOp<EBC_ClearEvalStack>::disassembly(code, so); break;
            case EBC_Jump: ByteCodeOp<EBC_Jump>::disassembly(code, so); break;
            case EBC_NJump: ByteCodeOp<EBC_NJump>::disassembly(code, so); break;
            case EBC_SetReturn: ByteCodeOp<EBC_SetReturn>::disassembly(code, so); break;
            case EBC_WriteLocal: ByteCodeOp<EBC_WriteLocal>::disassembly(code, so); break;
            case EBC_ReadLocal: ByteCodeOp<EBC_ReadLocal>::disassembly(code, so); break;
            case EBC_ReadConstant: ByteCodeOp<EBC_ReadConstant>::disassembly(code, so); break;
            case EBC_ArithmeticOp: ByteCodeOp<EBC_ArithmeticOp>::disassembly(code, so); break;
            case EBC_RelationOp: ByteCodeOp<EBC_RelationOp>::disassembly(code, so); break;
            case EBC_Not: ByteCodeOp<EBC_Not>::disassembly(code, so); break;
            case EBC_Call: ByteCodeOp<EBC_Call>::disassembly(code, so); break;
            case EBC_Push: ByteCodeOp<EBC_Push>::disassembly(code, so); break;
            case EBC_Pop: ByteCodeOp<EBC_Pop>::disassembly(code, so); break;
            default: ASSERT(0);
        }
    }
}
void ByteCodeSeq::run(StackFrame* frame)
{
    while (frame->ip < m_codes.size()) {
        int code = m_codes[frame->ip];
        switch (code >> 24) {
            case EBC_ClearEvalStack: ByteCodeOp<EBC_ClearEvalStack>::execute(code, frame); break;
            case EBC_Jump: ByteCodeOp<EBC_Jump>::execute(code, frame); break;
            case EBC_NJump: ByteCodeOp<EBC_NJump>::execute(code, frame); break;
            case EBC_SetReturn: ByteCodeOp<EBC_SetReturn>::execute(code, frame); break;
            case EBC_WriteLocal: ByteCodeOp<EBC_WriteLocal>::execute(code, frame); break;
            case EBC_ReadLocal: ByteCodeOp<EBC_ReadLocal>::execute(code, frame); break;
            case EBC_ReadConstant: ByteCodeOp<EBC_ReadConstant>::execute(code, frame); break;
            case EBC_ArithmeticOp: ByteCodeOp<EBC_ArithmeticOp>::execute(code, frame); break;
            case EBC_RelationOp: ByteCodeOp<EBC_RelationOp>::execute(code, frame); break;
            case EBC_Not: ByteCodeOp<EBC_Not>::execute(code, frame); break;
            case EBC_Call: ByteCodeOp<EBC_Call>::execute(code, frame); break;
            case EBC_Push: ByteCodeOp<EBC_Push>::execute(code, frame); break;
            case EBC_Pop: ByteCodeOp<EBC_Pop>::execute(code, frame); break;
            default: ASSERT(0);
        }
    }
}
