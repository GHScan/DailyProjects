
#ifndef STACKBASED_INTERPRETER_H
#define STACKBASED_INTERPRETER_H

#include "Interpreter.h"

class SB_InterpreterFactory: public InterpreterFactory {
public:
    SB_InterpreterFactory();
    virtual Interpreter* createInterpreter(const string &name);
    virtual InstructionList* createInstructionList();
};

#endif
