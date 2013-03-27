%token BREAK DO ELSE ELSEIF END FALSE 
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

static stack<LuaFunctionMetaPtr>* functionMetaStack();

namespace {

struct Temp_FuncNameInfo {
    bool includingSelf;
    vector<string> names;
    Temp_FuncNameInfo(): includingSelf(false){}
};

}

%}

%%

Program : Block;

Block
    : StatementList 
    | LastStatement 
    | StatementList StatementSep LastStatement 
    | 
    ;

StatementList
    : Statement 
    | StatementList StatementSep Statement
    ;

StatementSep : ';' | ;

Statement
    : VarList OP_ASSIGN ExpList
    | FunctionCall 
    | DO Block END
    | WHILE Exp DO Block END
    | REPEAT Block UNTIL Exp
    | IF Exp THEN Block END
    | IF Exp THEN Block ELSE Block END
    | IF Exp THEN Block ElseIfList END
    | IF Exp THEN Block ElseIfList ELSE Block END
    | FOR ID OP_ASSIGN Exp ',' Exp DO Block END
    | FOR ID OP_ASSIGN Exp ',' Exp ',' Exp DO Block END
    | FOR IDList IN ExpList DO Block END
    | FUNCTION FuncName FuncBody  {
        auto &info = $2.get<Temp_FuncNameInfo>();
        auto vars = vector<ExpNodePtr>();
        auto exps = vector<ExpNodePtr>();
        // refactor
        auto name = info.names[0];
        ExpNodePtr funcExp;
        auto localIdx = SymbolTable::top()->getLocalIndex(name);
        if (localIdx != -1) {
            funcExp = ExpNodePtr(new LocalVarExpNode(localIdx, functionMetaStack()->top()->getNameIndex(name)));
        } else {
            auto uvIdx = SymbolTable::top()->getUpValueIndex(name);
            if (uvIdx != -1) {
                funcExp = ExpNodePtr(new UpValueVarExpNode(uvIdx, functionMetaStack()->top()->getNameIndex(name)));
            } else {
                funcExp = ExpNodePtr(new GlobalVarExpNode(name));
            }
        }
        // FIXME:
        if (info.names.size() > 1) {
            for (int i = 1; i < (int)info.names.size(); ++i) { 
                int constIdx = functionMetaStack()->top()->getConstIndex(LuaValue(info.names[i].c_str()));
                funcExp = ExpNodePtr(new FieldAccessExpNode(funcExp, ExpNodePtr(new ConstExpNode(constIdx))));
            }
        }
        vars.push_back(funcExp);
        exps.push_back($3.get<ExpNodePtr>());
        $$ = StmtNodePtr(new AssignStmtNode(vars, exps));
    }
    | LOCAL FUNCTION ID FuncBody {
        string name = $3.get<string>();
        SymbolTable::top()->declareLocal(name);
        auto vars = vector<ExpNodePtr>();
        auto exps = vector<ExpNodePtr>();

        // TODO
        auto index = SymbolTable::top()->getLocalIndex(name);
        auto nameIdx = functionMetaStack()->top()->getNameIndex(name);
        vars.push_back(ExpNodePtr(new LocalVarExpNode(index, nameIdx)));
        exps.push_back($4.get<ExpNodePtr>());
        $$ = StmtNodePtr(new AssignStmtNode(vars, exps));
    }
    | LOCAL IDList {
        for (auto &name : $2.get<vector<string> >()) {
            SymbolTable::top()->declareLocal(name);
        }
    }
    | LOCAL IDList OP_ASSIGN ExpList {
        vector<ExpNodePtr> vars;
        for (auto &name : $2.get<vector<string> >()) {
            SymbolTable::top()->declareLocal(name);
            
            // TODO: refactor
            auto index = SymbolTable::top()->getLocalIndex(name);
            auto nameIdx = functionMetaStack()->top()->getNameIndex(name);
            vars.push_back(ExpNodePtr(new LocalVarExpNode(index, nameIdx)));
        }
        $$ = StmtNodePtr(new AssignStmtNode(vars, $4.get<vector<ExpNodePtr> >()));
    }
    ;

ElseIfList 
    : ElseIf {
        auto vec = vector<pair<ExpNodePtr, StmtNodePtr> >();
        vec.push_back($1.get<pair<ExpNodePtr, StmtNodePtr> >());
        $$ = vec;
    }
    | ElseIfList ElseIf {
        auto &vec = $1.get<vector<pair<ExpNodePtr, StmtNodePtr> > >();
        vec.push_back($2.get<pair<ExpNodePtr, StmtNodePtr> >());
        $$ = move($1);
    }
    ;
ElseIf : ELSEIF Exp THEN Block {
        $$ = make_pair($2.get<ExpNodePtr>(), $4.get<StmtNodePtr>());
       };

LastStatement 
    : RETURN Opt_ExpList {
        $$ = StmtNodePtr(new ReturnStmtNode($2.get<vector<ExpNodePtr> >()));
    }
    | BREAK {
        $$ = StmtNodePtr(new BreakStmtNode());
    }
    ;
        
FuncName 
    : DotIDList
    | DotIDList ':' ID {
        auto &info = $1.get<Temp_FuncNameInfo>();
        info.names.push_back($3.get<string>());
        info.includingSelf = true;
        $$ = info;
    }
    ;

DotIDList 
    : ID {
        auto info = Temp_FuncNameInfo();
        info.names.push_back($1.get<string>());
        $$ = info;
    }
    | DotIDList '.' ID {
        auto &info = $1.get<Temp_FuncNameInfo>();
        info.names.push_back($3.get<string>());
        $$ = info;
    }
    ;

IDList 
    : ID {
        auto ids = vector<string>();
        ids.push_back($1.get<string>());
        $$ = ids;
    }
    | IDList ',' ID {
        $1.get<vector<string> >().push_back($3.get<string>());
        $$ = move($1);
    }
    ;

Opt_ExpList 
    :  {
        $$ = vector<ExpNodePtr>();
    }
    | ExpList
    ;
ExpList 
    : Exp {
        auto v = vector<ExpNodePtr>();
        v.push_back($1.get<ExpNodePtr>());
        $$ = v;
    }
    | ExpList ',' Exp {
        $1.get<vector<ExpNodePtr> >().push_back($3.get<ExpNodePtr>());
        $$ = move($1);
    }
    ;
Exp 
    : NIL {
        int constIdx = functionMetaStack()->top()->getConstIndex(LuaValue::NIL);
        $$ = ExpNodePtr(new ConstExpNode(constIdx));
    }
    | FALSE {
        int constIdx = functionMetaStack()->top()->getConstIndex(LuaValue::FALSE);
        $$ = ExpNodePtr(new ConstExpNode(constIdx));
    }
    | TRUE {
        int constIdx = functionMetaStack()->top()->getConstIndex(LuaValue::TRUE);
        $$ = ExpNodePtr(new ConstExpNode(constIdx));
    }
    | Number
    | Literal
    | OP_ARGS {
        $$ = ExpNodePtr(new ArgsTupleExpNode());
    }
    | Lambda
    | PrefixExp
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
        $1.get<vector<ExpNodePtr> >().push_back($3.get<ExpNodePtr>());
        $$ = move($1);
    }
    ;
Var 
    : ID {
        auto name = $1.get<string>();
        auto localIdx = SymbolTable::top()->getLocalIndex(name);
        if (localIdx != -1) {
            $$ = ExpNodePtr(new LocalVarExpNode(localIdx, functionMetaStack()->top()->getNameIndex(name)));
        } else {
            auto uvIdx = SymbolTable::top()->getUpValueIndex(name);
            if (uvIdx != -1) {
                $$ = ExpNodePtr(new UpValueVarExpNode(uvIdx, functionMetaStack()->top()->getNameIndex(name)));
            } else {
                $$ = ExpNodePtr(new GlobalVarExpNode(name));
            }
        }
    }
    | PrefixExp '[' Exp ']' {
        auto table = $1.get<ExpNodePtr>();
        $$ = ExpNodePtr(new FieldAccessExpNode(table, $3.get<ExpNodePtr>()));
    }
    | PrefixExp '.' ID {
        auto table = $1.get<ExpNodePtr>();
        int constIdx =
        functionMetaStack()->top()->getConstIndex(LuaValue($3.get<string>().c_str()));
        $$ = ExpNodePtr(new FieldAccessExpNode(table, ExpNodePtr(new ConstExpNode(constIdx))));
    }
    ;

PrefixExp 
    : Var
    | FunctionCall  
    | '(' Exp ')' {
        $$ = $2;
    }
    ;

FunctionCall 
    : PrefixExp Params {
        $$ = ExpNodePtr(new CallExpNode($1.get<ExpNodePtr>(), move($2.get<vector<ExpNodePtr> >())));
    }
    | PrefixExp ':' ID Params {
        auto table = $1.get<ExpNodePtr>();
        auto constIdx = functionMetaStack()->top()->getConstIndex(LuaValue($3.get<string>().c_str()));
        $$ = ExpNodePtr(new CallExpNode(
            ExpNodePtr(new FieldAccessExpNode(table, ExpNodePtr(new ConstExpNode(constIdx)))),
            move($4.get<vector<ExpNodePtr> >())
        ));
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
        $$ = ExpNodePtr(new LambdaExpNode($2.get<LuaFunctionMetaPtr>()));
       }
       ;
FuncBody : '(' Opt_ArgList ')' {
            auto meta = LuaFunctionMetaPtr(new LuaFunctionMeta());
            functionMetaStack()->push(meta);
            SymbolTable::push();

            auto &args = $2.get<vector<string> >();
            meta->argCount = (int)args.size();
            for (auto &arg : args) {
                SymbolTable::top()->declareLocal(arg);
            }
         } Block END  {
            auto meta = functionMetaStack()->top();
            SymbolTable::pop();
            functionMetaStack()->pop();
            meta->body = $4.get<StmtNodePtr>();
            $$ = meta;
         }
         ;

Opt_ArgList 
    : {
        $$ = vector<string>();
    }
    | ArgList
    ;
ArgList 
    : IDList 
    | OP_ARGS {
        $$ = vector<string>();
    }
    | IDList ',' OP_ARGS {
        $$ = move($1);
    }
    ;

TableConstructor 
    : '{' Opt_FieldList '}' {
        $$ = $2;
    }
    ;

Opt_FieldList 
    : {
        $$ = ExpNodePtr(new TableConstructorExpNode);
    }
    | FieldList
    ;
FieldList 
    : Field {
        auto table = new TableConstructorExpNode();
        $$ = ExpNodePtr(table);
        if ($1.isTypeOf<ExpNodePtr>()) table->vec.push_back($1.get<ExpNodePtr>());
        else table->hashTable.push_back($1.get<pair<ExpNodePtr, ExpNodePtr> >());
    }
    | FieldList FieldSep Field {
        auto table = static_cast<TableConstructorExpNode*>($1.get<ExpNodePtr>().get());
        $$ = move($1);
        if ($2.isTypeOf<ExpNodePtr>()) table->vec.push_back($2.get<ExpNodePtr>());
        else table->hashTable.push_back($2.get<pair<ExpNodePtr, ExpNodePtr> >());
    }
    ;

FieldSep : ',' | ';' ;

Field 
    : Exp {
        $$ = ExpNodePtr($1.get<ExpNodePtr>());
    }
    | ID OP_ASSIGN Exp  {
        auto str = $1.get<string>();
        int constIdx = functionMetaStack()->top()->getConstIndex(LuaValue(str.c_str()));
        $$ = make_pair(ExpNodePtr(new ConstExpNode(constIdx)), $3.get<ExpNodePtr>());
    }
    | '[' Exp ']' OP_ASSIGN Exp {
        $$ = make_pair($2.get<ExpNodePtr>(), $5.get<ExpNodePtr>());
    }
    ;

BinOp_Exp 
    : Exp AND Exp {
        $$ = ExpNodePtr(new BinOpExpNode("and", $1.get<ExpNodePtr>(), $2.get<ExpNodePtr>()));
    }
    | Exp OR Exp {
        $$ = ExpNodePtr(new BinOpExpNode("or", $1.get<ExpNodePtr>(), $2.get<ExpNodePtr>()));
    }
    | Exp OP_LESS Exp {
        $$ = ExpNodePtr(new BinOpExpNode("<", $1.get<ExpNodePtr>(), $2.get<ExpNodePtr>()));
    }
    | Exp OP_LEQUAL Exp {
        $$ = ExpNodePtr(new BinOpExpNode("<=", $1.get<ExpNodePtr>(), $2.get<ExpNodePtr>()));
    }
    | Exp OP_GREATER Exp {
        $$ = ExpNodePtr(new BinOpExpNode(">", $1.get<ExpNodePtr>(), $2.get<ExpNodePtr>()));
    }
    | Exp OP_GEQUAL Exp {
        $$ = ExpNodePtr(new BinOpExpNode(">=", $1.get<ExpNodePtr>(), $2.get<ExpNodePtr>()));
    }
    | Exp OP_EQUAL Exp {
        $$ = ExpNodePtr(new BinOpExpNode("==", $1.get<ExpNodePtr>(), $2.get<ExpNodePtr>()));
    }
    | Exp OP_NEQUAL Exp {
        $$ = ExpNodePtr(new BinOpExpNode("~=", $1.get<ExpNodePtr>(), $2.get<ExpNodePtr>()));
    }
    | Exp OP_ADD Exp  {
        $$ = ExpNodePtr(new BinOpExpNode("+", $1.get<ExpNodePtr>(), $2.get<ExpNodePtr>()));
    }
    | Exp OP_SUB Exp  {
        $$ = ExpNodePtr(new BinOpExpNode("-", $1.get<ExpNodePtr>(), $2.get<ExpNodePtr>()));
    }
    | Exp OP_MUL Exp {
        $$ = ExpNodePtr(new BinOpExpNode("*", $1.get<ExpNodePtr>(), $2.get<ExpNodePtr>()));
    }
    | Exp OP_DIV Exp {
        $$ = ExpNodePtr(new BinOpExpNode("/", $1.get<ExpNodePtr>(), $2.get<ExpNodePtr>()));
    }
    | Exp OP_MOD Exp {
        $$ = ExpNodePtr(new BinOpExpNode("%", $1.get<ExpNodePtr>(), $2.get<ExpNodePtr>()));
    }
    | Exp OP_POWER Exp {
        $$ = ExpNodePtr(new BinOpExpNode("^", $1.get<ExpNodePtr>(), $2.get<ExpNodePtr>()));
    }
    | Exp OP_CONCAT Exp {
        $$ = ExpNodePtr(new BinOpExpNode("..", $1.get<ExpNodePtr>(), $2.get<ExpNodePtr>()));
    }
    ;
UnOp_Exp 
    : OP_SUB Exp {
        $$ = ExpNodePtr(new UnOpExpNode("-", $2.get<ExpNodePtr>()));
    }
    | NOT Exp {
        $$ = ExpNodePtr(new UnOpExpNode("not", $2.get<ExpNodePtr>()));
    }
    | OP_LEN Exp {
        $$ = ExpNodePtr(new UnOpExpNode("#", $2.get<ExpNodePtr>()));
    }
    ;

Number 
    : NUMBER1 {
        int num = 0;
        sscanf($1.get<string>().c_str(), "%x", &num);
        int constIdx = functionMetaStack()->top()->getConstIndex(LuaValue(NumberType(num)));
        $$ = ExpNodePtr(new ConstExpNode(constIdx));
    }
    | NUMBER2 {
        NumberType num = atof($1.get<string>().c_str());
        int constIdx = functionMetaStack()->top()->getConstIndex(LuaValue(num));
        $$ = ExpNodePtr(new ConstExpNode(constIdx));
    }
    | NUMBER3 {
        NumberType num = atof($1.get<string>().c_str());
        int constIdx = functionMetaStack()->top()->getConstIndex(LuaValue(num));
        $$ = ExpNodePtr(new ConstExpNode(constIdx));
    }
    ;
Literal
    : LITERAL1 {
        auto str = $1.get<string>();
        string realStr = str.substr(1, str.size() - 2);
        int constIdx = functionMetaStack()->top()->getConstIndex(LuaValue(realStr.c_str()));
        $$ = ExpNodePtr(new ConstExpNode(constIdx));
    }
    | LITERAL2 {
        auto str = $1.get<string>();
        string realStr = str.substr(1, str.size() - 2);
        int constIdx = functionMetaStack()->top()->getConstIndex(LuaValue(realStr.c_str()));
        $$ = ExpNodePtr(new ConstExpNode(constIdx));
    }
    | LITERAL3 {
        auto str = $1.get<string>();
        string realStr = str.substr(2, str.size() - 4);
        int constIdx = functionMetaStack()->top()->getConstIndex(LuaValue(realStr.c_str()));
        $$ = ExpNodePtr(new ConstExpNode(constIdx));
    }
    ;

%%

static stack<LuaFunctionMetaPtr>* functionMetaStack() {
    static stack<LuaFunctionMetaPtr> s_ins;
    return &s_ins;
}

bool parseFile(const char *fname) {
    bool succ = false;
    FILE *f = fopen(fname, "r");
    try {
        LuaFunctionMetaPtr meta(new LuaFunctionMeta());
        functionMetaStack()->push(meta);
        SymbolTable::push();

        yyrestart(f);
        yyparse();
        succ = true;

        SymbolTable::pop();
        functionMetaStack()->pop();
        LuaFunctionPtr func(LuaFunction::create(meta, vector<LuaValue>()));
        vector<LuaValue> rets;
        func->call(vector<LuaValue>(), rets);
    }
    catch(const exception& e) {
        printf("Catch parse exception: %s\n", e.what());
    }
    fclose(f);
    return succ;
}

