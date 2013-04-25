%token CONTINUE BREAK DO ELSE ELSEIF END FALSE 
%token FOR FUNCTION IF IN LOCAL NIL
%token REPEAT RETURN THEN TRUE UNTIL WHILE

%token OP_ARGS

%token LITERAL1 LITERAL2 LITERAL3
%token NUMBER1 NUMBER2 NUMBER3

%token ID

%right OP_ASSIGN

%left AND OR
%left OP_EQUAL OP_NEQUAL OP_LEQUAL OP_GEQUAL OP_LESS OP_GREATER
%left OP_ADD OP_SUB
%left OP_MUL OP_DIV OP_MOD
%left OP_POWER
%left OP_CONCAT

%nonassoc OP_LEN NOT

%{

#include "LuaFunction.h"
#include "SymbolTable.h"
#include "LuaValue.h"
#include "ByteCode.h"

static ExpNodePtr getConstExp(const LuaValue& v, int line);
static ExpNodePtr getIDExp(const TerminalSymbol& term);
static bool& isDefiningMethod();
static string& srcFileName();
static string unEscape(const string& s);

%}

%%

Program : Block {
            auto &meta = SymbolTable::top()->meta();
            meta->ast = $1.get<StmtNodePtr>();
            emitCode(meta.get());
        };

Block
    : StatementList {
        auto lrange = SymbolTable::top()->getBlockLocalRange();
        $$ = StmtNodePtr(new StmtNode_Block($1.get<vector<StmtNodePtr> >(), lrange.first, lrange.second));
    }
    | LastStatement {
        auto stmts = vector<StmtNodePtr>();
        stmts.push_back($1.get<StmtNodePtr>());
        auto lrange = SymbolTable::top()->getBlockLocalRange();
        $$ = StmtNodePtr(new StmtNode_Block(stmts, lrange.first, lrange.second));
    }
    | StatementList StatementSep LastStatement {
        auto& stmts = $1.get<vector<StmtNodePtr> >();
        stmts.push_back($3.get<StmtNodePtr>());
        auto lrange = SymbolTable::top()->getBlockLocalRange();
        $$ = StmtNodePtr(new StmtNode_Block(stmts, lrange.first, lrange.second));
    }
    | {
        auto lrange = SymbolTable::top()->getBlockLocalRange();
        $$ = StmtNodePtr(new StmtNode_Block(vector<StmtNodePtr>(), lrange.first, lrange.second));
    }
    ;

StatementList
    : Statement {
        auto vec = vector<StmtNodePtr>();
        vec.push_back($1.get<StmtNodePtr>());
        $$ = vec;
    }
    | StatementList StatementSep Statement {
        $1.get<vector<StmtNodePtr> >().push_back($3.get<StmtNodePtr>());
        $$ = move($1);
    }
    ;

StatementSep : ';' | ;

Statement
    : VarList OP_ASSIGN ExpList {
        $$ = StmtNodePtr(new StmtNode_Assign($1.get<vector<ExpNodePtr> >(), $3.get<vector<ExpNodePtr> >()));
    }

    | FunctionCall {
        $$ = StmtNodePtr(new StmtNode_Call($1.get<ExpNodePtr>()));
    }

    | DO {
        SymbolTable::top()->pushBlock();
    } Block END {
        $$ = move($3);
        SymbolTable::top()->popBlock();
    }

    | WHILE Exp DO {
        SymbolTable::top()->pushBlock();
    } Block END {
        SymbolTable::top()->popBlock();
        $$ = StmtNodePtr(new StmtNode_LoopFor(StmtNodePtr(), $2.get<ExpNodePtr>(), $5.get<StmtNodePtr>()));
    }

    | REPEAT {
        SymbolTable::top()->pushBlock();
    } Block UNTIL Exp {
        SymbolTable::top()->popBlock();
        auto &stmt = $3.get<StmtNodePtr>();
        auto exp = ExpNodePtr(new ExpNode_UnaryOp(ExpNode_UnaryOp::OP_Not, $5.get<ExpNodePtr>()));
        $$ = StmtNodePtr(new StmtNode_LoopFor(stmt, exp, stmt));
    }

    | IF Exp THEN {
        SymbolTable::top()->pushBlock();
    } Block {
        SymbolTable::top()->popBlock();
    } Opt_ElseIfList Opt_ElseBlock END  {
        auto ifExpStmts = vector<ExpStmtPair>();
        ifExpStmts.push_back(make_pair($2.get<ExpNodePtr>(), $5.get<StmtNodePtr>()));
        {
            auto& elseIfList = $7.get<vector<ExpStmtPair> >();
            ifExpStmts.insert(ifExpStmts.end(), elseIfList.begin(), elseIfList.end());
        }
        $$ = StmtNodePtr(new StmtNode_IfElse(ifExpStmts, $8.get<StmtNodePtr>()));
    }

    | FOR ID OP_ASSIGN Exp ',' Exp DO {
        SymbolTable::top()->pushBlock();
        SymbolTable::top()->declareLocal($2.get<TerminalSymbol>().lexem);
    } Block END {
        auto varExp = getIDExp($2.get<TerminalSymbol>());
        auto stmt = new StmtNode_RangeFor(varExp, $4.get<ExpNodePtr>(), $6.get<ExpNodePtr>(), getConstExp(LuaValue(1), 0), $9.get<StmtNodePtr>());
        $$ = StmtNodePtr(stmt);
        SymbolTable::top()->popBlock();
    }

    | FOR ID OP_ASSIGN Exp ',' Exp ',' Exp DO {
        SymbolTable::top()->pushBlock();
        SymbolTable::top()->declareLocal($2.get<TerminalSymbol>().lexem);
    } Block END {
        auto varExp = getIDExp($2.get<TerminalSymbol>());
        auto stmt = new StmtNode_RangeFor(varExp, $4.get<ExpNodePtr>(), $6.get<ExpNodePtr>(), $8.get<ExpNodePtr>(), $11.get<StmtNodePtr>());
        $$ = StmtNodePtr(stmt);
        SymbolTable::top()->popBlock();
    }

    | FOR IDList IN ExpList DO {
        SymbolTable::top()->pushBlock();
        for (auto &term : $2.get<vector<TerminalSymbol> >()) {
            SymbolTable::top()->declareLocal(term.lexem);
        }
        int stateLocalIdx = SymbolTable::top()->genInternalLocal("__state_");
        int funcLocalIdx = SymbolTable::top()->genInternalLocal("__func_");
        $1 = make_pair(stateLocalIdx, funcLocalIdx);
    } Block END {
        vector<ExpNodePtr> vars;
        for (auto &term : $2.get<vector<TerminalSymbol> >()) vars.push_back(getIDExp(term));
        auto stmt = new StmtNode_IteratorFor(vars, $4.get<vector<ExpNodePtr> >(), $7.get<StmtNodePtr>());
        stmt->stateLocalIdx = $1.get<pair<int, int> >().first;
        stmt->funcLocalIdx = $1.get<pair<int, int> >().second;
        $$ = StmtNodePtr(stmt);
        SymbolTable::top()->popBlock();
    }

    | FUNCTION FuncName FuncBody {
        vector<ExpNodePtr> lvalues; lvalues.push_back($2.get<ExpNodePtr>());
        vector<ExpNodePtr> rvalues; rvalues.push_back($3.get<ExpNodePtr>());
        $$ = StmtNodePtr(new StmtNode_Assign(lvalues, rvalues));
    }

    | LOCAL FUNCTION ID {
        auto& term = $3.get<TerminalSymbol>();
        SymbolTable::top()->declareLocal(term.lexem);
    } FuncBody {
        auto& term = $3.get<TerminalSymbol>();
        vector<ExpNodePtr> lvalues; lvalues.push_back(getIDExp(term));
        vector<ExpNodePtr> rvalues; rvalues.push_back($5.get<ExpNodePtr>());
        $$ = StmtNodePtr(new StmtNode_Assign(lvalues, rvalues));
    }

    | LOCAL IDList {
        for (auto &term : $2.get<vector<TerminalSymbol> >()) {
            SymbolTable::top()->declareLocal(term.lexem);
        }
        $$ = StmtNodePtr();
    }

    | LOCAL IDList OP_ASSIGN ExpList {
        auto &terms = $2.get<vector<TerminalSymbol> >();
        for (auto &term : terms) {
            SymbolTable::top()->declareLocal(term.lexem);
        }
        vector<ExpNodePtr> lvalues;
        for (auto &term : terms) lvalues.push_back(getIDExp(term));
        $$ = StmtNodePtr(new StmtNode_Assign(lvalues, $4.get<vector<ExpNodePtr> >()));
    }
    ;

Opt_ElseBlock
    : ElseBlock 
    | {
        $$ = StmtNodePtr();
    }
    ;
ElseBlock: ELSE {
            SymbolTable::top()->pushBlock();
         } Block {
            SymbolTable::top()->popBlock();
            $$ = $3.get<StmtNodePtr>();
         };

Opt_ElseIfList 
    : ElseIfList
    | {
        $$ = vector<ExpStmtPair>();
    }
    ;

ElseIfList 
    : ElseIf {
        auto vec = vector<ExpStmtPair>();
        vec.push_back($1.get<ExpStmtPair>());
        $$ = vec;
    }
    | ElseIfList ElseIf {
        $1.get<vector<ExpStmtPair> >().push_back($2.get<ExpStmtPair>());
        $$ = move($1);
    }
    ;
ElseIf : ELSEIF Exp THEN {
           SymbolTable::top()->pushBlock();
       } Block {
           SymbolTable::top()->popBlock();
           $$ = make_pair($2.get<ExpNodePtr>(), $5.get<StmtNodePtr>());
       };

LastStatement 
    : RETURN Opt_ExpList {
        $$ = StmtNodePtr(new StmtNode_Return($2.get<vector<ExpNodePtr> >()));
    }
    | BREAK {
        $$ = StmtNodePtr(new StmtNode_Break(getTokenLine()));
    }
    | CONTINUE {
        $$ = StmtNodePtr(new StmtNode_Continue(getTokenLine()));
    }
    ;
        
FuncName 
    : DotIDList
    | DotIDList ':' ID {
        auto &term = $3.get<TerminalSymbol>();
        $$ = ExpNodePtr(new ExpNode_FieldAccess($1.get<ExpNodePtr>(), getConstExp(LuaValue(term.lexem.c_str()), term.line)));
        isDefiningMethod() = true;
    }
    ;

DotIDList 
    : ID {
        $$ = getIDExp($1.get<TerminalSymbol>());
    }
    | DotIDList '.' ID {
        auto &term = $3.get<TerminalSymbol>();
        $$ = ExpNodePtr(new ExpNode_FieldAccess($1.get<ExpNodePtr>(), getConstExp(LuaValue(term.lexem.c_str()), term.line)));
    }
    ;

IDList 
    : ID {
        auto vec = vector<TerminalSymbol>();
        vec.push_back($1.get<TerminalSymbol>());
        $$ = vec;
    }
    | IDList ',' ID {
        $1.get<vector<TerminalSymbol> >().push_back($3.get<TerminalSymbol>());
        $$ = move($1);
    }
    ;

Opt_ExpList 
    : {
        $$ = vector<ExpNodePtr>();
    }
    | ExpList
    ;
ExpList 
    : Exp {
        auto vec = vector<ExpNodePtr>();
        vec.push_back($1.get<ExpNodePtr>());
        $$ = vec;
    }
    | ExpList ',' Exp {
        $1.get<vector<ExpNodePtr> >().push_back($3.get<ExpNodePtr>());
        $$ = move($1);
    }
    ;
Exp 
    : NIL {
        $$ = getConstExp(LuaValue::NIL, getTokenLine());
    }
    | FALSE {
        $$ = getConstExp(LuaValue::FALSE, getTokenLine());
    }
    | TRUE {
        $$ = getConstExp(LuaValue::TRUE, getTokenLine());
    }
    | Number
    | Literal
    | OP_ARGS {
        $$ = ExpNodePtr(new ExpNode_Args(getTokenLine()));
    }
    | Lambda
    | PrefixExp 
    | '(' Exp ')' {
        $$ = move($2);
    }
    | TableConstructor
    | BinOp_Exp 
    | UnOp_Exp
    ;

VarList 
    : Var {
        auto vec = vector<ExpNodePtr>();
        vec.push_back($1.get<ExpNodePtr>());
        $$ = vec;
    }
    | VarList ',' Var {
        auto& vec = $1.get<vector<ExpNodePtr> >();
        vec.push_back($3.get<ExpNodePtr>());
        $$ = move($1);
    }
    ;
Var 
    : ID {
        $$ = getIDExp($1.get<TerminalSymbol>());
    }
    | PrefixExp '[' Exp ']' {
        $$ = ExpNodePtr(new ExpNode_FieldAccess($1.get<ExpNodePtr>(), $3.get<ExpNodePtr>()));
    }
    | PrefixExp '.' ID {
        auto &term = $3.get<TerminalSymbol>();
        $$ = ExpNodePtr(new ExpNode_FieldAccess($1.get<ExpNodePtr>(), getConstExp(LuaValue(term.lexem.c_str()), term.line)));
    }
    ;

PrefixExp 
    : Var
    | FunctionCall  
    ;

FunctionCall 
    : PrefixExp Params {
        $$ = ExpNodePtr(new ExpNode_Call($1.get<ExpNodePtr>(), $2.get<vector<ExpNodePtr> >()));
    }
    | PrefixExp ':' ID Params {
        auto &term = $3.get<TerminalSymbol>();
        auto idExp = getConstExp(LuaValue(term.lexem.c_str()), term.line);
        auto funcExp = ExpNodePtr(new ExpNode_FieldAccess($1.get<ExpNodePtr>(), idExp));
        auto &params = $4.get<vector<ExpNodePtr> >();
        params.insert(params.begin(), $1.get<ExpNodePtr>());
        $$ = ExpNodePtr(new ExpNode_Call(funcExp, params));
    }
    ;

Params 
    : '(' Opt_ExpList ')' {
        $$ = move($2);
    }
    | Literal {
        auto vec = vector<ExpNodePtr>();
        vec.push_back($1.get<ExpNodePtr>());
        $$ = vec;
    }
    | TableConstructor {
        auto vec = vector<ExpNodePtr>();
        vec.push_back($1.get<ExpNodePtr>());
        $$ = vec;
    }
    ;

Lambda : FUNCTION FuncBody {
        $$ = move($2);
       }
FuncBody : '(' Opt_ArgList ')' {
            auto &args = $2.get<vector<TerminalSymbol> >();
            auto meta = LuaFunctionMetaPtr(new LuaFunctionMeta(srcFileName()));
            meta->argCount = (int)args.size();
            meta->line = args.empty() ? 0 : args[0].line;
            SymbolTable::push(meta);
            if (isDefiningMethod()) {
                isDefiningMethod() = false;
                SymbolTable::top()->declareLocal("self");
            }
            for (auto &term : args) {
                SymbolTable::top()->declareLocal(term.lexem);
            }
         } Block END {
            auto &meta = SymbolTable::top()->meta();
            meta->ast = $5.get<StmtNodePtr>();
            emitCode(meta.get());
            $$ = ExpNodePtr(new ExpNode_Lambda(meta));
            SymbolTable::pop();
         }

Opt_ArgList 
    : {
        $$ = vector<TerminalSymbol>();
    }
    | ArgList
    ;
ArgList 
    : IDList
    | OP_ARGS {
        $$ = vector<TerminalSymbol>();
    }
    | IDList ',' OP_ARGS 
    ;

TableConstructor 
    : '{' Opt_FieldList '}' {
        auto tableExp = new ExpNode_TableConstructor();
        $$ = ExpNodePtr(tableExp);
        auto &vec = $2.get<vector<Any> >();
        for (auto &v : vec) {
            if (v.isTypeOf<ExpNodePtr>()) {
                tableExp->array.push_back(v.get<ExpNodePtr>());
            } else {
                tableExp->dict.push_back(v.get<pair<ExpNodePtr, ExpNodePtr> >());
            }
        }
    }
    ;

Opt_FieldList 
    :  {
        $$ = vector<Any>();
    }
    | FieldList
    ;
FieldList 
    : Field {
        auto vec = vector<Any>();
        vec.push_back($1);
        $$ = vec;
    }
    | FieldList FieldSep Field {
        $1.get<vector<Any> >().push_back($3);
        $$ = move($1);
    }
    ;

FieldSep : ',' | ';' ;

Field 
    : Exp
    | ID OP_ASSIGN Exp {
        auto &term = $1.get<TerminalSymbol>();
        $$ = make_pair(getConstExp(LuaValue(term.lexem.c_str()), term.line), $3.get<ExpNodePtr>());
    }
    | '[' Exp ']' OP_ASSIGN Exp {
        $$ = make_pair($2.get<ExpNodePtr>(), $5.get<ExpNodePtr>());
    }
    ;

BinOp_Exp 
    : Exp AND Exp  {
        $$ = ExpNodePtr(new ExpNode_BinaryOp(ExpNode_BinaryOp::OP_And, $1.get<ExpNodePtr>(), $3.get<ExpNodePtr>()));
    }
    | Exp OR Exp {
        $$ = ExpNodePtr(new ExpNode_BinaryOp(ExpNode_BinaryOp::OP_Or, $1.get<ExpNodePtr>(), $3.get<ExpNodePtr>()));
    }
    | Exp OP_LESS Exp {
        $$ = ExpNodePtr(new ExpNode_BinaryOp(ExpNode_BinaryOp::OP_Less, $1.get<ExpNodePtr>(), $3.get<ExpNodePtr>()));
    }
    | Exp OP_LEQUAL Exp {
        $$ = ExpNodePtr(new ExpNode_BinaryOp(ExpNode_BinaryOp::OP_LessEq, $1.get<ExpNodePtr>(), $3.get<ExpNodePtr>()));
    }
    | Exp OP_GREATER Exp {
        $$ = ExpNodePtr(new ExpNode_BinaryOp(ExpNode_BinaryOp::OP_Greater, $1.get<ExpNodePtr>(), $3.get<ExpNodePtr>()));
    }
    | Exp OP_GEQUAL Exp  {
        $$ = ExpNodePtr(new ExpNode_BinaryOp(ExpNode_BinaryOp::OP_GreaterEq, $1.get<ExpNodePtr>(), $3.get<ExpNodePtr>()));
    }
    | Exp OP_EQUAL Exp {
        $$ = ExpNodePtr(new ExpNode_BinaryOp(ExpNode_BinaryOp::OP_Equal, $1.get<ExpNodePtr>(), $3.get<ExpNodePtr>()));
    }
    | Exp OP_NEQUAL Exp {
        $$ = ExpNodePtr(new ExpNode_BinaryOp(ExpNode_BinaryOp::OP_NEqual, $1.get<ExpNodePtr>(), $3.get<ExpNodePtr>()));
    }
    | Exp OP_ADD Exp {
        $$ = ExpNodePtr(new ExpNode_BinaryOp(ExpNode_BinaryOp::OP_Add, $1.get<ExpNodePtr>(), $3.get<ExpNodePtr>()));
    }
    | Exp OP_SUB Exp {
        $$ = ExpNodePtr(new ExpNode_BinaryOp(ExpNode_BinaryOp::OP_Sub, $1.get<ExpNodePtr>(), $3.get<ExpNodePtr>()));
    }
    | Exp OP_MUL Exp {
        $$ = ExpNodePtr(new ExpNode_BinaryOp(ExpNode_BinaryOp::OP_Mul, $1.get<ExpNodePtr>(), $3.get<ExpNodePtr>()));
    }
    | Exp OP_DIV Exp {
        $$ = ExpNodePtr(new ExpNode_BinaryOp(ExpNode_BinaryOp::OP_Div, $1.get<ExpNodePtr>(), $3.get<ExpNodePtr>()));
    }
    | Exp OP_MOD Exp {
        $$ = ExpNodePtr(new ExpNode_BinaryOp(ExpNode_BinaryOp::OP_Mod, $1.get<ExpNodePtr>(), $3.get<ExpNodePtr>()));
    }
    | Exp OP_POWER Exp {
        $$ = ExpNodePtr(new ExpNode_BinaryOp(ExpNode_BinaryOp::OP_Pow, $1.get<ExpNodePtr>(), $3.get<ExpNodePtr>()));
    }
    | Exp OP_CONCAT Exp {
        $$ = ExpNodePtr(new ExpNode_BinaryOp(ExpNode_BinaryOp::OP_Concat, $1.get<ExpNodePtr>(), $3.get<ExpNodePtr>()));
    }
    ;
UnOp_Exp 
    : OP_SUB Exp {
        $$ = ExpNodePtr(new ExpNode_UnaryOp(ExpNode_UnaryOp::OP_Minus, $2.get<ExpNodePtr>()));
    }
    | NOT Exp  {
        $$ = ExpNodePtr(new ExpNode_UnaryOp(ExpNode_UnaryOp::OP_Not, $2.get<ExpNodePtr>()));
    }
    | OP_LEN Exp {
        $$ = ExpNodePtr(new ExpNode_UnaryOp(ExpNode_UnaryOp::OP_Len, $2.get<ExpNodePtr>()));
    }
    ;

Number 
    : NUMBER1 {
        auto &term = $1.get<TerminalSymbol>();
        int num = 0;
        sscanf(term.lexem.c_str(), "%x", &num);
        $$ = getConstExp(LuaValue(num), term.line);
    }
    | NUMBER2 {
        auto &term = $1.get<TerminalSymbol>();
        double num = 0;
        sscanf(term.lexem.c_str(), "%lf", &num);
        $$ = getConstExp(LuaValue(num), term.line);
    }
    | NUMBER3 {
        auto &term = $1.get<TerminalSymbol>();
        double num = 0;
        sscanf(term.lexem.c_str(), "%lf", &num);
        $$ = getConstExp(LuaValue(num), term.line);
    }
    ;
Literal
    : LITERAL1 {
        auto& term = $1.get<TerminalSymbol>();
        $$ = getConstExp(
            LuaValue(unEscape(term.lexem.substr(1, term.lexem.size() - 2)).c_str()), term.line);
    }
    | LITERAL2 {
        auto& term = $1.get<TerminalSymbol>();
        $$ = getConstExp(
            LuaValue(unEscape(term.lexem.substr(1, term.lexem.size() - 2)).c_str()), term.line);
    }
    | LITERAL3 {
        auto& term = $1.get<TerminalSymbol>();
        $$ = getConstExp(
            LuaValue(term.lexem.substr(2, term.lexem.size() - 4).c_str()), term.line);
    }
    ;

%%

LuaValue _loadFile(FILE *f) {
    try {
        auto meta = LuaFunctionMetaPtr(new LuaFunctionMeta(srcFileName()));
        SymbolTable::push(meta);
        yyrestart(f);
        yyparse();
        SymbolTable::pop();
        return LuaValue(LuaFunction::create(meta));
    } catch(Exception&) {
        SymbolTable::pop();
        throw;
    }
}
LuaValue loadFile(FILE *f) {
    srcFileName() = "[????]";
    return _loadFile(f);
}
LuaValue loadFile(const char *name) {
    srcFileName() = name;
    FILE *f = fopen(name, "r");
    ASSERT1(f != NULL, "Couldn't open the file");
    try {
        auto r = _loadFile(f);
        fclose(f);
        return r;
    } catch(const exception& ) {
        fclose(f);
        throw;
    }
}

static ExpNodePtr getConstExp(const LuaValue& v, int line) {
    int constIdx = SymbolTable::top()->meta()->getConstIdx(v);
    return ExpNodePtr(new ExpNode_Const(constIdx, line));
}
static ExpNodePtr getIDExp(const TerminalSymbol& term) {
    int localIdx = SymbolTable::top()->getLocalIdx(term.lexem);
    if (localIdx != -1) return ExpNodePtr(new ExpNode_LocalVar(localIdx, term.line));
    int uvIdx = SymbolTable::top()->getUpValueIdx(term.lexem);
    if (uvIdx != -1) return ExpNodePtr(new ExpNode_UpValueVar(uvIdx, term.line));
    int constIdx = SymbolTable::top()->meta()->getConstIdx(LuaValue(term.lexem.c_str()));
    return ExpNodePtr(new ExpNode_GlobalVar(constIdx, term.line));
}
static bool& isDefiningMethod() {
    static bool s_b = false;
    return s_b;
}
static string& srcFileName() {
    static string s_fname;
    return s_fname;
}
static string unEscape(const string& s) {
    string r;
    for (int i = 0; i < (int)s.size(); ++i) {
        if (s[i] == '\\') {
            switch (s[i + 1]) {
            case 'a': r.push_back('\a'); break;
            case 'n': r.push_back('\n'); break;
            case 'r': r.push_back('\r'); break;
            case 't': r.push_back('\t'); break;
            default: r.push_back(s[i + 1]); break;
            }
            ++i;
        } else {
            r.push_back(s[i]);
        }
    }
    return r;
}
