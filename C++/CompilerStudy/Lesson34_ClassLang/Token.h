#ifndef TOKEN_H
#define TOKEN_H

class Atom;

struct Token {
    Atom *lexeme;
    int line : 20;
    int column : 12;
};

#endif
