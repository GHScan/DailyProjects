#include "pch.h"

#include "Utils.h"
#include "StackBasedISA.h"
#include "StackBasedInterpreter.h"

int main(int argc, char *argv[]) {
    SB_setupISA();

    for (int i = 1; i < argc; ++i) {
        SB_InstructionList insList;
        {
            ifstream fi(argv[i]);
            if (!fi) continue;
            fi >> insList;
            insList.convertJmpOff();
        }
        printf("=============== %s ===============\n", argv[i]);

        const char *ints[] = {
            "call", "switch", "repl_switch", "token", "direct", "jit",
        };
        for (int i = 0; i < COUNT_OF_A(ints); ++i) {
            SB_Interpreter *p = SB_Interpreter::getInstance(ints[i]);
            if (p->isValid()) { 
                clock_t start = clock();
                int res = p->interpret(&insList);
                float time = float(clock() - start) / CLOCKS_PER_SEC;
                printf("%s : res=%d, time=%g\n", ints[i], res, time);
            } else {
                printf("%s : invalid\n", ints[i]);
            }
        }
    }

    return 0;
}
