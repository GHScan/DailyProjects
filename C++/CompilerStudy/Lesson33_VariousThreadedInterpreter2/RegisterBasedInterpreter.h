
#ifndef REGISTER_BASED_INTERPRETER_H
#define REGISTER_BASED_INTERPRETER_H

#include "Interpreter.h"

class RB_InterpreterFactory: public InterpreterFactory {
public:
    RB_InterpreterFactory();
    virtual Interpreter* createInterpreter(const string &name);
    virtual InstructionList* createInstructionList();
};

#endif
