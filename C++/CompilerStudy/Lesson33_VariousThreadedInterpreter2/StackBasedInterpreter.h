
#ifndef STACKBASED_INTERPRETER_H
#define STACKBASED_INTERPRETER_H

#include "Interpreter.h"

class InterpreterFactory_SB: public InterpreterFactory {
public:
    virtual Interpreter* createInterpreter(const string &name);
};

#endif
