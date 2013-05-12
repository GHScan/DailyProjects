tree grammar TreeExpr;

options {
    language = Python;
    tokenVocab=Expr;
    ASTLabelType = CommonTree;
}

@init {
    self.m_memory = dict()
}

prog : stat+;
stat 
    : expr { print $expr.value }
    | ^('=' ID expr) { self.m_memory[$ID.text] = $expr.value; }
    ;

expr returns [value]
    : NUMBER { value = int($NUMBER.text) }
    | ID { value = self.m_memory[$ID.text]; }
    | ^('+' a=expr b=expr) { value = $a.value + $a.value }
    | ^('-' a=expr b=expr) { value = $a.value - $a.value }
    | ^('*' a=expr b=expr) { value = $a.value * $a.value }
    | ^('/' a=expr b=expr) { value = $a.value / $a.value }
    | ^('%' a=expr b=expr) { value = $a.value \% $a.value }
    ;
