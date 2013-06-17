
#ifndef LLVM_COMPILER_H
#define LLVM_COMPILER_H

struct SourceFileProto;
typedef shared_ptr<SourceFileProto> SourceFileProtoPtr;

class LLVMCompiler {
public:
    LLVMCompiler(const SourceFileProtoPtr &proto);
    ~LLVMCompiler();
    void compile(bool doOptimize);
    void print(const string& fileName);
    void run();

private:
    class LLVMCompilerImpl *m_impl;
};

#endif
