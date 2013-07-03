
#ifndef BE_x86_ASM_SERIALIZER_H
#define BE_x86_ASM_SERIALIZER_H

class BEx86FileBuilder;

void serializex86Code_nasm(ostream &so, BEx86FileBuilder *fileBuilder);

#endif
