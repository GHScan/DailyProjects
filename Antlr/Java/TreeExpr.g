tree grammar TreeExpr;

options {
    tokenVocab = Expr;
    ASTLabelType = CommonTree;
}

@header {
    import java.util.HashMap;
}

@members {
    HashMap m_memory = new HashMap();
}

prog : stat+;
stat 
    : expr {
        System.out.println($expr.value);
    }
    | ^('=' ID expr) {
        m_memory.put($ID.text, $expr.value);
    }
    ;
 
expr returns [int value]
    : NUMBER { value = Integer.parseInt($NUMBER.text); }
    | ID { value = (int)m_memory.get($ID.text); }
    | ^('+' a=expr b=expr) { value = $a.value + $b.value;}
    | ^('-' a=expr b=expr) { value = $a.value - $b.value;}
    | ^('*' a=expr b=expr) { value = $a.value * $b.value;}
    | ^('/' a=expr b=expr) { value = $a.value / $b.value;}
    | ^('%' a=expr b=expr) { value = $a.value \% $b.value;}
    ;
