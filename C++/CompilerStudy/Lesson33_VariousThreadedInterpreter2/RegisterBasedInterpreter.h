
#ifndef REGISTER_BASED_INTERPRETER_H
#define REGISTER_BASED_INTERPRETER_H


class RB_InstructionList;

class RB_Interpreter {
public:
    static RB_Interpreter* getInstance(const string &name);
    virtual ~RB_Interpreter(){}

    virtual int interpret(RB_InstructionList *insList) = 0;
    virtual bool isValid() = 0;
private:
};

#endif
