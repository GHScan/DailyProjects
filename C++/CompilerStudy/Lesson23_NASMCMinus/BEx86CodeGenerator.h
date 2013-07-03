
#ifndef BE_x86_CODE_GENERATOR_H
#define BE_x86_CODE_GENERATOR_H

class BEx86FileBuilder;
struct SourceFileProto;

BEx86FileBuilder* generatex86Code(SourceFileProto *fileProto);

#endif
