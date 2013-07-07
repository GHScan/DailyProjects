
#ifndef AST_OPTIMIZER_H
#define AST_OPTIMIZER_H

struct SourceFileProto;

enum ASTOptimizeType {
    AOT_ConstantFolding = 1 << 0,
    AOT_ReductionInStrength = 1 << 1,
    AOT_AlgebraicIdentity = 1 << 2,
    AOT_ErshovNumber = 1 << 3,

    AOT_All = 0xf,
    AOT_Count = 4,
};

void optimizeAST(SourceFileProto *fileProto, ASTOptimizeType optTypeFlag);

#endif
