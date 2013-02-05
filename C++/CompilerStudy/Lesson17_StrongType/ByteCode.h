
#ifndef BYTE_CODE_H
#define BYTE_CODE_H

class RuntimeEnv;
class ByteCodeSeq
{
public:
    ByteCodeSeq(StmtNodePtr stmt);
    ~ByteCodeSeq();

    int getFrameSize() { return m_frameSize;}
    void disassemble(ostream& so);
    void execute(RuntimeEnv *env);
private:
    vector<int> m_codes;
    int m_frameSize;
};


#endif
