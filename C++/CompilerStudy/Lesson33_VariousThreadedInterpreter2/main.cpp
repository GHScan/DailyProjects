#include "pch.h"

#include "Utils.h"
#include "Interpreter.h"

int main(int argc, char *argv[]) {
    for (int i = 1; i < argc; ++i) {
        InterpreterFactory *factory = NULL;
        InstructionList *insList = NULL;
        {
            ifstream fi(argv[i]);
            if (!fi) {
                printf("invalid file: %s!\n", argv[i]); 
                continue;
            }

            string intName;
            fi >> intName;
            factory = InterpreterFactory::getFactory(intName);
            if (factory == NULL) {
                printf("invalid interpreter: %s!\n", intName.c_str()); 
                continue;
            }

            insList = factory->createInstructionList();
            insList->fromStream(fi);
            insList->translateJmpIdx2Off();
        }
        printf("=============== %s ===============\n", argv[i]);

        const char *ints[] = {
            "call", "switch", "repl_switch", "token", "direct", "jit",
        };
        for (int i = 0; i < COUNT_OF_A(ints); ++i) {
            Interpreter *p = factory->createInterpreter(ints[i]);
            if (p->isValid()) { 
                clock_t start = clock();
                int res = p->interpret(insList);
                float time = float(clock() - start) / CLOCKS_PER_SEC;
                printf("%s : res=%d, time=%g\n", ints[i], res, time);
            } else {
                printf("%s : invalid\n", ints[i]);
            }
            factory->destroyInterpreter(p);
        }

        factory->destroyInstructionList(insList);
    }

    return 0;
}
