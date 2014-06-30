#ifndef SDISASSEMBLER_H
#define SDISASSEMBLER_H

struct SDisassembler {
    static void disassemble(ostream &so, int indent, uint8_t *codes, int codeSize);
    static uint8_t* disassembleIns(ostream &so, uint8_t *ins);
};

#endif
