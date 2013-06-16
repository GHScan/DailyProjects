
#ifndef LLVM_COMPILER_H
#define LLVM_COMPILER_H

struct SourceFileProto;
typedef shared_ptr<SourceFileProto> SourceFileProtoPtr;

class LLVMCompiler {
public:
    LLVMCompiler(const SourceFileProtoPtr &proto);
    void compile();
    void run();

private:
};

#endif
