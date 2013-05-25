grammar Expr;

options {
    language = CSharp3;
}

@header {
}

@members {
     Dictionary<string, int> m_memory = new Dictionary<string, int>();
}

public prog : stat+;
stat
    : expr NEWLINE { System.Console.WriteLine($expr.value); }
    | ID '=' expr NEWLINE { m_memory[$ID.text] = $expr.value; }
    | NEWLINE 
    ;
expr returns[int value]
    : a=mulexpr { value = $a.value; } 
    ('+'  b=mulexpr { value += $b.value ;}
            | '-'  b=mulexpr { value -= $b.value ;} )*
    ;
mulexpr returns[int value]
    : a=atom { value = $a.value; }
    ('*' b=atom { value *= $b.value; }
            | '/' b=atom  { value /= $b.value; }
            | '%' b=atom { value \%= $b.value;}) *
    ;
atom returns[int value]
    : ID { 
        if (m_memory.ContainsKey($ID.text)) {
            value = m_memory[$ID.text];
        } else {
            throw new System.Exception($ID.text + " is not exist!");
        }
    }
    | NUMBER { value = int.Parse($NUMBER.text); }
    | '(' expr ')' { value = $expr.value;}
    ;

ID : ALPHA (ALPHA | DIGIT)*;
NUMBER : DIGIT+;
fragment ALPHA : 'a' .. 'z' | 'A' .. 'Z';
fragment DIGIT : '0' .. '9';
WS : ('\t' | ' ' ) {$channel=Hidden;};
NEWLINE : '\r'? '\n';
