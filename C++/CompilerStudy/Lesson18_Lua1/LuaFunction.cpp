
#include "pch.h"

#include "Ast.h"
#include "LuaFunction.h"
#include "Runtime.h"
#include "LuaTable.h"

static vector<LuaValue> evalExps(LuaFunction *func, vector<ExpNodePtr>& exps) ;

class ExpNodeVisitor_Eval:
    public IExpNodeVisitor {
public:
    ExpNodeVisitor_Eval(LuaFunction *func): m_func(func){}
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
            m_rets.push_back(LuaValue(-value.getNumber()));
        } else if (v->op == "not") {
            if (value.isTypeOf(LVT_Nil) || !value.getBoolean()) {
                m_rets.push_back(LuaValue::TRUE);
            } else m_rets.push_back(LuaValue::FALSE);
        } else if (v->op == "#") {
            m_rets.push_back(LuaValue(value.getSize()));
        } else ASSERT(0);
    }
    virtual void visit(ConstExpNode *v) {
        m_rets.push_back(m_func->getMeta()->constTable[v->index]);
    }
    virtual void visit(LocalVarExpNode *v) {
        m_rets.push_back(m_func->getLocal(v->index));
    }
    virtual void visit(UpValueVarExpNode *v) {
        m_rets.push_back(m_func->getUpValue(v->index));
    }
    virtual void visit(GlobalVarExpNode *v) {
        m_rets.push_back(Runtime::instance()->getGlobalTable()->get(LuaValue(v->name)));
    }
    virtual void visit(FieldAccessExpNode *v) {
        v->table->acceptVisitor(this);
        auto table = m_rets[0]; m_rets.clear();
        v->field->acceptVisitor(this);
        auto field = m_rets[0]; m_rets.clear();
        m_rets.push_back(table.get(field)); 
    }
    virtual void visit(TableConstructorExpNode *v) {
        LuaValue table(LVT_Table);
        {
            auto vals(evalExps(m_func, v->vec));
            for (int i = 0; i < (int)vals.size(); ++i) {
                table.set(LuaValue(i + 1), vals[i]);
            }
        }
        for (int i = 0; i < (int)v->hashTable.size(); ++i) {
            v->hashTable[i].first->acceptVisitor(this);
            LuaValue k = m_rets[0]; m_rets.clear();
            v->hashTable[i].second->acceptVisitor(this);
            LuaValue v = m_rets[0]; m_rets.clear();
            table.set(k, v);
        }
        m_rets.push_back(table);
    }
    virtual void visit(LambdaExpNode *v) {
        // FIXME:
        vector<LuaValue> upValues;
        m_rets.push_back(LuaValue(LVT_Function, (int)LuaFunction::create(v->meta, upValues)));
    }
    virtual void visit(CallExpNode *v) {
        auto func = ExpNodeVisitor_Eval(m_func).apply(v->func)[0].getFunction();
        vector<LuaValue> params(evalExps(m_func, v->params)); 
        func->call(params, m_rets);
    }
    virtual void visit(ArgsTupleExpNode *v) {
        auto &args = m_func->getArgs();
        m_rets.assign(args.begin() + m_func->getMeta()->argCount, args.end());
    }
private:
    LuaFunction *m_func;
    vector<LuaValue> m_rets;
};

static vector<LuaValue> evalExps(LuaFunction *func, vector<ExpNodePtr>& exps) {
    vector<LuaValue> r;
    for (int i = 0; i < (int)exps.size() - 1; ++i) {
        r.push_back(ExpNodeVisitor_Eval(func).apply(exps[i])[0]);
    }
    auto vec(move(ExpNodeVisitor_Eval(func).apply(exps.back())));
    r.insert(r.end(), vec.begin(), vec.end());
    return r;
}

class StmtNodeVisitor_Execute:
    public IStmtNodeVisitor {
public:
    StmtNodeVisitor_Execute(LuaFunction* func): m_func(func), m_isBreak(false), m_isReturn(false){}
    vector<LuaValue>& apply() {
        m_func->getMeta()->body->acceptVisitor(this);
        return m_rets;
    }
private:
    virtual void visit(CallStmtNode *v) {
        ExpNodeVisitor_Eval(m_func).apply(v->exp);
    }
    virtual void visit(AssignStmtNode *v) {
        auto values = evalExps(m_func, v->exps);
        for (int i = 0; i < (int)v->vars.size(); ++i) {
            if (auto localExp = dynamic_cast<LocalVarExpNode*>(v->vars[i].get())) {
                m_func->getLocal(localExp->index) = values[i];
            }
            else if (auto upValueExp = dynamic_cast<UpValueVarExpNode*>(v->vars[i].get())) {
                m_func->getUpValue(upValueExp->index) = values[i];
            }
            else {
                assert(dynamic_cast<FieldAccessExpNode*>(v->vars[i].get()));
                auto fieldAccessExp = static_cast<FieldAccessExpNode*>(v->vars[i].get());
                ExpNodeVisitor_Eval(m_func).apply(fieldAccessExp->table)[0].set(
                        ExpNodeVisitor_Eval(m_func).apply(fieldAccessExp->field)[0], values[i]);
            }
        }
    }
    virtual void visit(BreakStmtNode *v) {
        m_isBreak = true;
    }
    virtual void visit(ReturnStmtNode *v) {
        m_isReturn = true;
        m_rets = move(evalExps(m_func, v->exps));
    }
    virtual void visit(BlockStmtNode *v) {
        for (auto &stmt : v->stmts) {
            stmt->acceptVisitor(this);
            if (m_isBreak || m_isReturn) break;
        }
    }
    virtual void visit(IfElseStmtNode *v) {
        bool ok = false;
        for (auto &p : v->ifExpStmtList) {
            if (ExpNodeVisitor_Eval(m_func).apply(p.first)[0].getBoolean()) {
                ok = true;
                p.second->acceptVisitor(this);
                break;
            }
        }
        if (!ok && v->elseStmt) {
            v->elseStmt->acceptVisitor(this);
        }
    }
    virtual void visit(RangeForStmtNode *v) {
        LuaValue cur = ExpNodeVisitor_Eval(m_func).apply(v->first)[0];
        LuaValue last = ExpNodeVisitor_Eval(m_func).apply(v->first)[1];
        LuaValue step = ExpNodeVisitor_Eval(m_func).apply(v->first)[2];
        while (cur <= last) {
            m_func->getLocal(v->index) = cur;
            v->stmt->acceptVisitor(this);
            cur += step;
            if (m_isBreak || m_isReturn) break;
        }
        m_isBreak = false;
    }
    virtual void visit(LoopForStmtNode *v) {
        if (v->stmtPre != NULL) v->stmtPre->acceptVisitor(this);
        while (ExpNodeVisitor_Eval(m_func).apply(v->exp)[0].getBoolean()) {
            v->stmtBody->acceptVisitor(this);
            if (m_isBreak || m_isReturn) break;
        }
        m_isBreak = false;
    }
    virtual void visit(IteraterForStmtNode *v) {
        LuaValue func, state, k;
        {
            auto iterValues(evalExps(m_func, v->iterExps));
            assert(iterValues.size() >= 1);
            if (iterValues.size() >= 1) func = iterValues[0];
            else if (iterValues.size() >= 2) state = iterValues[1];
            else if (iterValues.size() >= 3) k = iterValues[2];
            else {}
        }
        for (;;) {
            vector<LuaValue> args;
            args.push_back(state); args.push_back(k);
            vector<LuaValue> rets;
            func.getFunction()->call(args, rets);
            k = rets[0];
            if (k == LuaValue::NIL) break;
            for (int i = 0; i < (int)v->indexs.size(); ++i) {
                m_func->getLocal(v->indexs[i]) = rets[i];
            }
            v->stmt->acceptVisitor(this);
            if (m_isBreak || m_isReturn) break;
        }
        m_isBreak = false;
    }
private:
    LuaFunction *m_func;
    vector<LuaValue> m_rets;
    bool m_isBreak, m_isReturn;
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
LuaFunction::LuaFunction(LuaFunctionMetaPtr meta, const vector<LuaValue>& upValues): 
    m_meta(meta), m_upValues(upValues)  {
}
LuaFunction::~LuaFunction() {
}
void LuaFunction::call(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    m_args = args;
    m_locals.assign(m_args.begin(), m_args.end());
    rets = move(StmtNodeVisitor_Execute(this).apply());
}
LuaValue& LuaFunction::getLocal(int idx) {
    if (idx < (int)m_locals.size()) return m_locals[idx];
    m_locals.resize(idx + 1);
    return m_locals[idx];
}
LuaValue& LuaFunction::getUpValue(int idx) {
    return m_upValues[idx];
}
