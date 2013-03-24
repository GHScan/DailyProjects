
#include "pch.h"

#include "Ast.h"
#include "LuaFunction.h"

class ExpNodeVisitor_Eval:
    public IExpNodeVisitor {
public:
    ExpNodeVisitor_Eval(LuaFunction *func): m_func(func){}
    LuaValue& apply(ExpNodePtr& exp) {
        exp->acceptVisitor(this);
        return m_ret;
    }
private:
    virtual void visit(BinOpExpNode *v) {
    }
    virtual void visit(UnOpExpNode *v) {
    }
    virtual void visit(ConstExpNode *v) {
    }
    virtual void visit(LocalVarExpNode *v) {
    }
    virtual void visit(UpValueVarExpNode *v) {
    }
    virtual void visit(GlobalVarExpNode *v) {
    }
    virtual void visit(FieldAccessExpNode *v) {
    }
    virtual void visit(TableConstructorExpNode *v) {
    }
    virtual void visit(LambdaExpNode *v) {
    }
    virtual void visit(CallExpNode *v) {
    }
    virtual void visit(ArgsTupleExpNode *v) {
    }
private:
    LuaFunction *m_func;
    LuaValue m_ret;
};

class StmtNodeVisitor_Execute:
    public IStmtNodeVisitor {
public:
    StmtNodeVisitor_Execute(LuaFunction* func, const vector<LuaValue>& args): m_func(func), m_args(args){}
    vector<LuaValue>& apply() {
        m_func->getMeta()->body->acceptVisitor(this);
        return m_rets;
    }
private:
    virtual void visit(CallStmtNode *v) {
        auto exp = static_cast<CallExpNode*>(v->exp.get());
        auto func = ExpNodeVisitor_Eval(m_func).apply(exp->func).getFunction();
        vector<LuaValue> params; 
        vector<LuaValue> rets;
        for (auto &paramExp : exp->params) params.push_back(ExpNodeVisitor_Eval(m_func).apply(paramExp));
        func->call(params, rets);
    }
    virtual void visit(AssignStmtNode *v) {
        if (v->vars.size() == 1 && dynamic_cast<LocalVarExpNode*>(v->vars[0].get())) {
            auto p = static_cast<LocalVarExpNode*>(v->vars[0].get());
            if (p->getName(m_func->getMeta()) == "return") {
                for (auto &exp : v->exps) {
                    m_rets.push_back(ExpNodeVisitor_Eval(m_func).apply(exp));
                }
            }
        }
        for (int i = 0; i < (int)v->vars.size(); ++i) {
            if (auto localExp = dynamic_cast<LocalVarExpNode*>(v->vars[i].get())) {
                m_func->getLocal(localExp->index) = ExpNodeVisitor_Eval(m_func).apply(v->exps[i]);
            }
            else if (auto upValueExp = dynamic_cast<UpValueVarExpNode*>(v->vars[i].get())) {
                m_func->getUpValue(upValueExp->index) = ExpNodeVisitor_Eval(m_func).apply(v->exps[i]);
            }
            else {
                assert(dynamic_cast<FieldAccessExpNode*>(v->vars[i].get()));
                auto fieldAccessExp = static_cast<FieldAccessExpNode*>(v->vars[i].get());
                ExpNodeVisitor_Eval(m_func).apply(fieldAccessExp->value).set(
                        ExpNodeVisitor_Eval(m_func).apply(fieldAccessExp->field), ExpNodeVisitor_Eval(m_func).apply(v->exps[i]));
            }
        }
    }
    virtual void visit(BlockStmtNode *v) {
        for (auto &stmt : v->stmts) {
            stmt->acceptVisitor(this);
        }
    }
    virtual void visit(IfElseStmtNode *v) {
        bool ok = false;
        for (auto &p : v->ifExpStmtList) {
            if (ExpNodeVisitor_Eval(m_func).apply(p.first).getBoolean()) {
                ok = true;
                p.second->acceptVisitor(this);
            }
        }
        if (!ok && v->elseStmt) {
            v->elseStmt->acceptVisitor(this);
        }
    }
    virtual void visit(RangeForStmtNode *v) {
        for (int i = v->first; i <= v->last; i += v->step) {
            m_func->getLocal(v->index) = LuaValue(i);
            v->stmt->acceptVisitor(this);
        }
    }
    virtual void visit(LoopForStmtNode *v) {
        v->stmtPre->acceptVisitor(this);
        while (ExpNodeVisitor_Eval(m_func).apply(v->exp).getBoolean()) {
            v->stmtBody->acceptVisitor(this);
        }
    }
    virtual void visit(IteraterForStmtNode *v) {
        // TODO: eval can return multi value!
        auto func = ExpNodeVisitor_Eval(m_func).apply(v->iterExp);
        LuaValue state, k;
        for (;;) {
            vector<LuaValue> args;
            vector<LuaValue> rets;
            func.getFunction()->call(args, rets);
            k = rets[0];
            if (k == LuaValue::NIL) break;
            m_func->getLocal(v->indexs[0]) = k;
            // FIXME
            v->stmt->acceptVisitor(this);
        }
    }
private:
private:
    LuaFunction *m_func;
    vector<LuaValue> m_args;
    vector<LuaValue> m_rets;
};

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
LuaFunction::LuaFunction(LuaFunctionMeta *meta, const vector<LuaValue>& upValues): 
    m_refCount(1), m_meta(meta), m_upValues(upValues)  {
}
LuaFunction::~LuaFunction() {
}
void LuaFunction::call(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    rets = StmtNodeVisitor_Execute(this, args).apply();
}
LuaValue& LuaFunction::getLocal(int idx) {
    if (idx < (int)m_locals.size()) return m_locals[idx];
    m_locals.resize(idx + 1);
    return m_locals[idx];
}
LuaValue& LuaFunction::getUpValue(int idx) {
    return m_upValues[idx];
}
