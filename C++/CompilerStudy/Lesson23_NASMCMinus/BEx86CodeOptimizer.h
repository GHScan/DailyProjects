
#ifndef BE_x86_CODE_OPTIMIZER
#define BE_x86_CODE_OPTIMIZER

class BEx86FileBuilder;

enum x86InstructionOptimizeType {
    x86IOT_DeadCode = 1 << 0,

    x86IOT_All = 1,
    x86IOT_Count = 1,
};

void optimizex86Code(BEx86FileBuilder *builder, int optTypeFlag);

#endif
