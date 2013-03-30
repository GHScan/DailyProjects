
#include "pch.h"

#include "Ast.h"
#include "LuaFunction.h"
#include "Runtime.h"
#include "LuaTable.h"

static string guessFunctionName(const LuaFunctionMeta* meta, CallExpNode *exp) {
    string name = "?";
    if (auto p = dynamic_cast<LocalVarExpNode*>(exp->func.get())){
        name = p->getName(meta);
    } else if (auto p = dynamic_cast<UpValueVarExpNode*>(exp->func.get())) {
        name = p->getName(meta);
    } else if (auto p = dynamic_cast<GlobalVarExpNode*>(exp->func.get())) {
        name = p->name;
    } else if (auto p = dynamic_cast<FieldAccessExpNode*>(exp->func.get())) {
        if (auto p2 = dynamic_cast<ConstExpNode*>(p->field.get())) {
            auto cst = meta->constTable[p2->index];
            if (cst.isTypeOf(LVT_String)) name = cst.getString();
        }
    }
    return name;
}

class StmtNodeVisitor_Execute:
    public IStmtNodeVisitor {
public:
    StmtNodeVisitor_Execute(LuaFunction* func): m_func(func), m_isBreak(false), m_isReturn(false){}
    vector<LuaValue>& apply(const vector<LuaValue>& args) {
        m_locals.assign(args.begin(), args.begin() + m_func->getMeta()->argCount);
        m_args = args;
        m_func->getMeta()->body->acceptVisitor(this);
        return m_rets;
    }
    LuaValue& getLocal(int idx) {
        if (idx < (int)m_locals.size()) return m_locals[idx];
        m_locals.resize(idx + 1);
        return m_locals[idx];
    }
    const vector<LuaValue>& getArgs() const {
        return m_args;
    }
    LuaFunction* getFunc() {
        return m_func;
    }
private:
    virtual void visit(CallStmtNode *v);
    virtual void visit(AssignStmtNode *v);
    virtual void visit(BreakStmtNode *v);
    virtual void visit(ReturnStmtNode *v);
    virtual void visit(BlockStmtNode *v);
    virtual void visit(IfElseStmtNode *v);
    virtual void visit(RangeForStmtNode *v);
    virtual void visit(LoopForStmtNode *v);
    virtual void visit(IteraterForStmtNode *v);
private:
    LuaFunction *m_func;
    vector<LuaValue> m_rets;
    vector<LuaValue> m_args;
    vector<LuaValue> m_locals;
    bool m_isBreak, m_isReturn;
};

static vector<LuaValue> evalExps(StmtNodeVisitor_Execute *stmt, vector<ExpNodePtr>& exps) ;

class ExpNodeVisitor_Eval:
    public IExpNodeVisitor {
public:
    ExpNodeVisitor_Eval(StmtNodeVisitor_Execute *stmt): m_stmt(stmt){}
    vector<LuaValue>& apply(ExpNodePtr& exp) {
        exp->acceptVisitor(this);
        return m_rets;
    }
private:
    virtual void visit(BinOpExpNode *v) {
        v->left->acceptVisitor(this);
        LuaValue lv = m_rets[0]; m_rets.clear();
        if (v->op == "and") {
            if (lv.getBoolean()) {
                v->right->acceptVisitor(this);
                LuaValue rv = m_rets[0]; m_rets.clear();
                m_rets.push_back(rv);
            } else m_rets.push_back(lv);
        } else if (v->op == "or") {
            if (!lv.getBoolean()) {
                v->right->acceptVisitor(this);
                LuaValue rv = m_rets[0]; m_rets.clear();
                m_rets.push_back(rv);
            }
            else m_rets.push_back(lv);
        }

        v->right->acceptVisitor(this);
        LuaValue rv = m_rets[0]; m_rets.clear();
        if (v->op == "<") {
            m_rets.push_back(lv < rv ? LuaValue::TRUE : LuaValue::FALSE);
        } else if (v->op == "<=") {
            m_rets.push_back(lv <= rv ? LuaValue::TRUE : LuaValue::FALSE);
        } else if (v->op == ">") {
            m_rets.push_back(lv > rv ? LuaValue::TRUE : LuaValue::FALSE);
        } else if (v->op == ">=") {
            m_rets.push_back(lv >= rv ? LuaValue::TRUE : LuaValue::FALSE);
        } else if (v->op == "==") {
            m_rets.push_back(lv == rv ? LuaValue::TRUE : LuaValue::FALSE);
        } else if (v->op == "~=") {
            m_rets.push_back(lv != rv ? LuaValue::TRUE : LuaValue::FALSE);
        } else if (v->op == "+") {
            m_rets.push_back(lv + rv);
        } else if (v->op == "-") {
            m_rets.push_back(lv - rv);
        } else if (v->op == "*") {
            m_rets.push_back(lv * rv);
        } else if (v->op == "/") {
            m_rets.push_back(lv / rv);
        } else if (v->op == "%") {
            m_rets.push_back(lv % rv);
        } else if (v->op == "^") {
            m_rets.push_back(lv.power(rv));
        } else if (v->op == "..") {
            m_rets.push_back(lv.concat(rv));
        } else ASSERT(0);
    }
    virtual void visit(UnOpExpNode *v) {
        v->exp->acceptVisitor(this);
        LuaValue value = m_rets[0]; m_rets.clear();
        if (v->op == "-") {
            if (value.isTypeOf(LVT_Table)) {
                m_rets.push_back(value.getTable()->meta_unm());
            } else m_rets.push_back(LuaValue(-value.getNumber()));
        } else if (v->op == "not") {
            if (!value.getBoolean()) m_rets.push_back(LuaValue::TRUE);
            else m_rets.push_back(LuaValue::FALSE);
        } else if (v->op == "#") {
            m_rets.push_back(LuaValue(NumberType(value.getSize())));
        } else ASSERT(0);
    }
    virtual void visit(ConstExpNode *v) {
        m_rets.push_back(m_stmt->getFunc()->getMeta()->constTable[v->index]);
    }
    virtual void visit(LocalVarExpNode *v) {
        m_rets.push_back(m_stmt->getLocal(v->index));
    }
    virtual void visit(UpValueVarExpNode *v) {
        m_rets.push_back(m_stmt->getFunc()->getUpValue(v->index));
    }
    virtual void visit(GlobalVarExpNode *v) {
        m_rets.push_back(m_stmt->getFunc()->getfenv()->get(LuaValue(v->name)));
    }
    virtual void visit(FieldAccessExpNode *v) {
        v->table->acceptVisitor(this);
        auto table = m_rets[0].getTable(); m_rets.clear();
        v->field->acceptVisitor(this);
        auto field = m_rets[0]; m_rets.clear();
        m_rets.push_back(table->get(field)); 
    }
    virtual void visit(TableConstructorExpNode *v) {
        auto table = LuaTable::create();
        {
            auto vals(evalExps(m_stmt, v->vec));
            for (int i = 0; i < (int)vals.size(); ++i) {
                table->set(LuaValue(NumberType(i + 1)), vals[i]);
            }
        }
        for (int i = 0; i < (int)v->hashTable.size(); ++i) {
            v->hashTable[i].first->acceptVisitor(this);
            LuaValue k = m_rets[0]; m_rets.clear();
            v->hashTable[i].second->acceptVisitor(this);
            LuaValue v = m_rets[0]; m_rets.clear();
            table->set(k, v);
        }
        m_rets.push_back(LuaValue(table));
    }
    virtual void visit(LambdaExpNode *v) {
        // FIXME:
        vector<LuaValue> upValues;
        m_rets.push_back(LuaValue(LuaFunction::create(v->meta, upValues)));
    }
    virtual void visit(CallExpNode *v) {
        v->func->acceptVisitor(this);

        try {
            auto func = m_rets[0]; m_rets.clear();
            vector<LuaValue> params(evalExps(m_stmt, v->params)); 
            if (func.isTypeOf(LVT_Table)) {
                func.getTable()->meta_call(params, m_rets);
            } else func.getFunction()->call(params, m_rets);
        } catch(Exception& e) {
            e.addLine(format("%s:%d: in function '%s'", 
                        v->srcFile.c_str(), v->srcLine, 
                        guessFunctionName(m_stmt->getFunc()->getMeta(), v).c_str()));
            throw;
        }
    }
    virtual void visit(ArgsTupleExpNode *v) {
        auto &args = m_stmt->getArgs();
        m_rets.assign(args.begin() + m_stmt->getFunc()->getMeta()->argCount, args.end());
    }
private:
    StmtNodeVisitor_Execute *m_stmt;
    vector<LuaValue> m_rets;
};

static vector<LuaValue> evalExps(StmtNodeVisitor_Execute *stmt, vector<ExpNodePtr>& exps) {
    vector<LuaValue> r;
    for (int i = 0; i < (int)exps.size() - 1; ++i) {
        r.push_back(ExpNodeVisitor_Eval(stmt).apply(exps[i])[0]);
    }
    if (exps.size() > 0) {
        auto vec(move(ExpNodeVisitor_Eval(stmt).apply(exps.back())));
        r.insert(r.end(), vec.begin(), vec.end());
    }
    return r;
}
void StmtNodeVisitor_Execute::visit(CallStmtNode *v) {
    ExpNodeVisitor_Eval(this).apply(v->exp);
}
void StmtNodeVisitor_Execute::visit(AssignStmtNode *v) {
    auto values = evalExps(this, v->exps);
    for (int i = 0; i < (int)v->vars.size(); ++i) {
        LuaValue value;
        if (i < (int)values.size()) value = values[i];
        if (auto localExp = dynamic_cast<LocalVarExpNode*>(v->vars[i].get())) {
            getLocal(localExp->index) = value;
        }
        else if (auto upValueExp = dynamic_cast<UpValueVarExpNode*>(v->vars[i].get())) {
            m_func->getUpValue(upValueExp->index) = value;
        }
        else if (auto gValueExp = dynamic_cast<GlobalVarExpNode*>(v->vars[i].get())) {
            // TODO: performance
            getFunc()->getfenv()->set(LuaValue(gValueExp->name), value);
        }
        else {
            assert(dynamic_cast<FieldAccessExpNode*>(v->vars[i].get()));
            auto fieldAccessExp = static_cast<FieldAccessExpNode*>(v->vars[i].get());
            ExpNodeVisitor_Eval(this).apply(fieldAccessExp->table)[0].getTable()->set(
                    ExpNodeVisitor_Eval(this).apply(fieldAccessExp->field)[0], value);
        }
    }
}
void StmtNodeVisitor_Execute::visit(BreakStmtNode *v) {
    m_isBreak = true;
}
void StmtNodeVisitor_Execute::visit(ReturnStmtNode *v) {
    m_isReturn = true;
    m_rets = move(evalExps(this, v->exps));
}
void StmtNodeVisitor_Execute::visit(BlockStmtNode *v) {
    for (auto &stmt : v->stmts) {
        stmt->acceptVisitor(this);
        if (m_isBreak || m_isReturn) break;
    }
}
void StmtNodeVisitor_Execute::visit(IfElseStmtNode *v) {
    bool ok = false;
    for (auto &p : v->ifExpStmtList) {
        if (ExpNodeVisitor_Eval(this).apply(p.first)[0].getBoolean()) {
            ok = true;
            p.second->acceptVisitor(this);
            break;
        }
    }
    if (!ok && v->elseStmt) {
        v->elseStmt->acceptVisitor(this);
    }
}
void StmtNodeVisitor_Execute::visit(RangeForStmtNode *v) {
    LuaValue cur = ExpNodeVisitor_Eval(this).apply(v->first)[0];
    LuaValue last = ExpNodeVisitor_Eval(this).apply(v->last)[0];
    LuaValue step = ExpNodeVisitor_Eval(this).apply(v->step)[0];
    assert(step.getNumber() != 0);
    while ((step.getNumber() > 0 && cur <= last) ||
            (step.getNumber() < 0 && cur >= last)) {
        getLocal(v->index) = cur;
        v->stmt->acceptVisitor(this);
        cur += step;
        if (m_isBreak || m_isReturn) break;
    }
    m_isBreak = false;
}
void StmtNodeVisitor_Execute::visit(LoopForStmtNode *v) {
    if (v->stmtPre != NULL) v->stmtPre->acceptVisitor(this);
    while (ExpNodeVisitor_Eval(this).apply(v->exp)[0].getBoolean()) {
        v->stmtBody->acceptVisitor(this);
        if (m_isBreak || m_isReturn) break;
    }
    m_isBreak = false;
}
void StmtNodeVisitor_Execute::visit(IteraterForStmtNode *v) {
    LuaValue func, state, k;
    {
        auto iterValues(evalExps(this, v->iterExps));
        assert(iterValues.size() >= 1);
        if (iterValues.size() >= 1) func = iterValues[0];
        if (iterValues.size() >= 2) state = iterValues[1];
        if (iterValues.size() >= 3) k = iterValues[2];
    }
    for (;;) {
        vector<LuaValue> args;
        args.push_back(state); args.push_back(k);
        vector<LuaValue> rets;
        func.getFunction()->call(args, rets);
        k = rets[0];
        if (k == LuaValue::NIL) break;
        for (int i = 0; i < (int)v->indexs.size(); ++i) {
            getLocal(v->indexs[i]) = rets[i];
        }
        v->stmt->acceptVisitor(this);
        if (m_isBreak || m_isReturn) break;
    }
    m_isBreak = false;
}

//======== LuaFunctionMeta ============
int LuaFunctionMeta::getConstIndex(const LuaValue& v) {
    for (int i = 0; i < (int)constTable.size(); ++i) {
        if (v == constTable[i]) return i;
    }
    int r = (int)constTable.size();
    constTable.push_back(v);
    return r;
}
int LuaFunctionMeta::getNameIndex(const string& name) {
    for (int i = 0; i < (int)nameTable.size(); ++i) {
        if (nameTable[i] == name) return i;
    }
    int r = (int)nameTable.size();
    nameTable.push_back(name);
    return r;
}
//======== LuaFunction ============
LuaFunction::LuaFunction(LuaFunctionMetaPtr meta, const vector<LuaValue>& upValues): 
    m_meta(meta), m_upValues(upValues)  {
}
LuaFunction::~LuaFunction() {
}
void LuaFunction::call(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    Runtime::instance()->pushFrame(this);
    rets = move(StmtNodeVisitor_Execute(this).apply(args));
    Runtime::instance()->popFrame();
}
bool LuaFunction::equal(IFunction *o) {
    if (auto p = dynamic_cast<LuaFunction*>(o)) {
        return m_meta == p->m_meta && m_upValues == p->m_upValues;
    }
    return false;
}
LuaValue& LuaFunction::getUpValue(int idx) {
    return m_upValues[idx];
}
