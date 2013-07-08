
#ifndef BE_x86_CODE_GENERATOR_H
#define BE_x86_CODE_GENERATOR_H

class BEx86FileBuilder;
struct SourceFileProto;
typedef shared_ptr<BEx86FileBuilder> BEx86FileBuilderPtr;

void generatex86Code(BEx86FileBuilder *fileBuilder, SourceFileProto *fileProto);

#endif
