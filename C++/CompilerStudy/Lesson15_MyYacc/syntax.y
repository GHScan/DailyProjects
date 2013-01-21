%token IF FOR WHILE DO BREAK CONTINUE RETURN LOCAL FUNCTION
%token INT STRING ID

%nonassoc LOWER_THAN_ELSE
%nonassoc ELSE
%right ASSIGN_OP
%left AND_OP 
%left CMP_OP
%left ADD_OP
%left MUL_OP
%nonassoc UNARY_OP
%nonassoc INC_OP

%%

Exp : Add 
    ;

Add: Add '+' Mul
   | Add '-' Mul
   | Mul
   ;
Mul: Mul '*' Term
   | Mul '/' Term
   | Term
   ;
Term : INT
     | '(' Exp ')'
     ;

%%

