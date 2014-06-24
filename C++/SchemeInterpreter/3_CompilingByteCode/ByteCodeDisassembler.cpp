#include "pch.h"
#include "ByteCodeDisassembler.h"
#include "SymbolTable.h"
#include "ByteCodeDefinition.h"

static void writeTab(ostream &so, int indent) {
    for (int i = 0; i < indent; ++i) so << '\t';
}

static string getFreeName(const SScriptFunctionProto *proto, int freeIndex) {
    auto address = proto->freeAddresses[freeIndex];
    for (int i = 0; i < address.getEnvIndex(); ++i) {
        proto = proto->parent;
    }
    return proto->locals[address.getVarIndex()];
}

void disassembleByteCode(
        ostream& so,
        const SScriptFunctionProto *proto,
        SymbolTable *gSymTable,
        const vector<SScriptFunctionProtoPtr> &protos,
        const vector<SValue> &literals,
        int indent) {

    writeTab(so, indent); so << "formals:\n";
    writeTab(so, indent + 1);
    for (int i = 0; i < proto->formalCount; ++i)  {
        so << proto->locals[i] << ',';
    }
    so << '\n';

    if (proto->formalCount < (int)proto->locals.size()) {
        writeTab(so, indent); so << "locals:\n";
        writeTab(so, indent + 1);
        for (int i = proto->formalCount; i < (int)proto->locals.size(); ++i)  {
            so << proto->locals[i] << ',';
        }
        so << '\n';
    }

    writeTab(so, indent); so << "code:\n";

    const uint8_t *bytes = &proto->bytes[0];
    for (int i = 0; i < (int)proto->bytes.size();) {
        writeTab(so, indent + 1); so << format("%3d ", i);

        switch (bytes[i]) {
            case ByteCode_LoadLiteral::CODE:
                so << "loadK ";
                literals[static_cast<const ByteCode_LoadLiteral*>((void*)&bytes[i])->literalIndex].writeToStream(so);
                i += sizeof(ByteCode_LoadLiteral);
                break;
            case ByteCode_LoadLocal::CODE:
                so << "loadL ";
                so << proto->locals[static_cast<const ByteCode_LoadLocal*>((void*)&bytes[i])->localIndex];
                i += sizeof(ByteCode_LoadLocal);
                break;
            case ByteCode_StoreLocal::CODE:
                so << "storeL ";
                so << proto->locals[static_cast<const ByteCode_StoreLocal*>((void*)&bytes[i])->localIndex];
                i += sizeof(ByteCode_StoreLocal);
                break;
            case ByteCode_LoadGlobal::CODE:
                so << "loadG ";
                so << gSymTable->getSymbolByIndex(static_cast<const ByteCode_LoadGlobal*>((void*)&bytes[i])->globalIndex);
                i += sizeof(ByteCode_LoadGlobal);
                break;
            case ByteCode_StoreGlobal::CODE:
                so << "storeG ";
                so << gSymTable->getSymbolByIndex(static_cast<const ByteCode_StoreGlobal*>((void*)&bytes[i])->globalIndex);
                i += sizeof(ByteCode_StoreGlobal);
                break;
            case ByteCode_LoadFree::CODE:
                so << "loadF ";
                so << getFreeName(proto, static_cast<const ByteCode_LoadFree*>((void*)&bytes[i])->freeIndex);
                i += sizeof(ByteCode_LoadFree);
                break;
            case ByteCode_StoreFree::CODE:
                so << "storeF ";
                so << getFreeName(proto, static_cast<const ByteCode_StoreFree*>((void*)&bytes[i])->freeIndex);
                i += sizeof(ByteCode_StoreFree);
                break;
            case ByteCode_LoadLambda::CODE:
                so << "loadLambda ";
                {
                    auto subProto = protos[static_cast<const ByteCode_LoadLambda*>((void*)&bytes[i])->protoIndex].get();
                    disassembleByteCode(so, subProto, gSymTable, protos, literals, indent + 1);
                }
                i += sizeof(ByteCode_LoadLambda);
                break;
            case ByteCode_Jmp::CODE:
                so << "jmp ";
                so << static_cast<const ByteCode_Jmp*>((void*)&bytes[i])->target;
                i += sizeof(ByteCode_Jmp);
                break;
            case ByteCode_TrueJmp::CODE:
                so << "tjmp ";
                so << static_cast<const ByteCode_TrueJmp*>((void*)&bytes[i])->target;
                i += sizeof(ByteCode_TrueJmp);
                break;
            case ByteCode_Tail::CODE:
                so << "tail ";
                i += sizeof(ByteCode_Tail);
                break;
            case ByteCode_Call::CODE:
                so << "call ";
                so << static_cast<const ByteCode_Call*>((void*)&bytes[i])->actualCount;
                i += sizeof(ByteCode_Call);
                break;
            case ByteCode_Pop::CODE:
                so << "pop ";
                i += sizeof(ByteCode_Pop);
                break;
            default:
                ASSERT(0);
                break;
        }

        so << '\n';
    }

    so.flush();
}
