#ifndef SCMINTERPRETER_H
#define SCMINTERPRETER_H

class ScmInterpreter {
public:
    ScmInterpreter();
    ~ScmInterpreter();

    void interpret(istream &si);

    ScmInterpreter(const ScmInterpreter&) = delete;
    ScmInterpreter& operator = (const ScmInterpreter&) = delete;

private:
};

#endif
