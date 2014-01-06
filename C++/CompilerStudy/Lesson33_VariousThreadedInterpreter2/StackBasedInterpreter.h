
#ifndef STACKBASED_INTERPRETER_H
#define STACKBASED_INTERPRETER_H

class SB_InstructionList;

class SB_Interpreter {
public:
    static SB_Interpreter* getInstance(const string &name);
    virtual ~SB_Interpreter(){}

    virtual int interpret(SB_InstructionList *insList) = 0;
    virtual bool isValid() = 0;
private:
};

#endif
