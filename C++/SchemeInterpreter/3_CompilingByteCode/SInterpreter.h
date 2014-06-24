#ifndef SINTERPRETER_H
#define SINTERPRETER_H

class SInterpreter {
public:
    SInterpreter();
    ~SInterpreter();

    void interpret(istream &si);

    SInterpreter(const SInterpreter&);
    SInterpreter& operator = (const SInterpreter&);

private:
    class SInterpreterImpl *mImpl;
};

#endif
