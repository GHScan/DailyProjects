#include "pch.h"
//============================== 
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#include <string>
#include <vector>
#include <set>
#include <map>
using namespace std;
//============================== 
#define ASSERT(b) do { if(!(b)) throw "assert failed ! " #b; } while(0)
#define ARRAY_SIZE(a) (sizeof(a) / sizeof(a[0]))
static string unescape(const string &s) {
    string r;
    for (int i = 0; i < (int)s.size(); ++i) {
        if (s[i] == '\\') {
            switch (s[++i]) {
                case 't': r += '\t'; break;
                case 'n': r += '\n'; break;
                case 'r': r += '\r'; break;
                default: r += s[i]; break;
            }
        } else r += s[i];
    }
    return r;
}
//============================== lexical analysis
enum TokenType {
    TT_LP, TT_RP, TT_LBRACE, TT_RBRACE, TT_LBRACKET, TT_RBRACKET, TT_COMMA, TT_SEMICELON,
    TT_IF, TT_ELSE, TT_FOR, TT_WHILE, TT_CONTINUE, TT_BREAK, TT_RETURN,
    TT_OP_NOT, TT_OP_INC, TT_OP_DEC,
    TT_OP_ASSIGN,
    TT_OP_AND, TT_OP_OR,
    TT_OP_ADD, TT_OP_SUB, TT_OP_MUL, TT_OP_DIV, TT_OP_MOD,   
    TT_OP_LESS, TT_OP_LESSEQ, TT_OP_GREATER, TT_OP_GREATEREQ, TT_OP_EQUAL, TT_OP_NEQUAL, 
    TT_TYPE_INT, TT_TYPE_CHARPTR, TT_TYPE_VOID,
    // special 
    TT_INT, TT_ID, TT_STRING,
    TT_EOF,
};
struct Token {
    TokenType tt;
    string lexeme;
    Token(): tt(TT_EOF) {}
    explicit Token(TokenType _tt): tt(_tt) {}
    Token(TokenType _tt, const char *begin, const char *end): tt(_tt), lexeme(begin, end){}
};
static map<string, Token> setupBuildinTokens() {
    map<string, Token> tokens;
    const char *lexemes[] = {
        "(", ")", "{", "}", "[", "]", ",", ";",
        "if", "else", "for", "continue", "break", "return",
        "!", "++", "--",
        "=",
        "&&", "||",
        "+", "-", "*", "/", "%", 
        "<", "<=", ">", ">=", "==", "!=",
        "int", "char*", "void",
    };
    for (int i = 0; i < ARRAY_SIZE(lexemes); ++i) tokens[lexemes[i]] = Token((TokenType)i);
    return tokens; 
}
static Token* getBuildinToken(const string &lexeme) {
    static map<string, Token> s_tokens(setupBuildinTokens());
    map<string, Token>::iterator iter = s_tokens.find(lexeme);
    return iter == s_tokens.end() ? NULL : &iter->second;
}
class Scanner { 
public:
    explicit Scanner(const char *src): m_src(src) { }
    Token LA(int n) { makesureN(n); return m_LAList[n - 1]; }
    Token next(int n) {
        makesureN(n);
        Token token = m_LAList[0];
        m_LAList.erase(m_LAList.begin(), m_LAList.begin() + n);
        return token;
    }
private:
    void makesureN(int n) {
        while ((int)m_LAList.size() < n) {
            for (; isspace(*m_src); ++m_src);
            if (m_src[0] == '/' && m_src[1] == '/') {
                while (*++m_src != '\n');
                for (; isspace(*m_src); ++m_src);
            }

            if (m_src[0] == 0) {
                m_LAList.push_back(Token());
            } else if (isdigit(m_src[0])) {
                const char *begin = m_src;
                while (isdigit(*++m_src));
                m_LAList.push_back(Token(TT_INT, begin, m_src));
            } else if (m_src[0] == '"') {
                const char *begin = m_src;
                while (*++m_src != '"') {
                    if (*m_src == '\\') ++m_src;
                }
                m_LAList.push_back(Token(TT_STRING, begin, ++m_src));
            } else if (isalpha(m_src[0]) || m_src[0] == '_') {
                const char *begin = m_src;
                do { ++m_src; } while(isalpha(m_src[0]) || m_src[0] == '_' || isdigit(m_src[0]));
                string lexeme(begin, m_src);
                if (Token *token = getBuildinToken(lexeme)) m_LAList.push_back(*token);
                else m_LAList.push_back(Token(TT_ID, begin, m_src));
            } else {
                string lexeme(m_src, m_src + 2);
                if (Token *token = getBuildinToken(lexeme)) {
                    m_LAList.push_back(*token);
                    m_src += 2;
                } else {
                    lexeme.assign(m_src, m_src + 1);
                    if (Token *token = getBuildinToken(lexeme)) {
                        m_LAList.push_back(*token);
                        ++m_src;
                    } else ASSERT(0 && "invalid token");
                }
            }
        }
    }
private:
    vector<Token> m_LAList;
    const char *m_src;
};
//============================== platform details for jit
#ifdef _MSC_VER
#pragma warning(disable : 4312)
#pragma warning(disable : 4311)
#include <Windows.h>
#include <Psapi.h>
#pragma warning(default : 4311)
#pragma warning(default : 4312)
static char* os_mallocExecutable(int size) {
    char *p = ::VirtualAlloc(NULL, size, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
    ASSERT(p != NULL);
    return p;
}
static void os_freeExecutable(char *p) {
    ASSERT(p != NULL);
	::VirtualFree(p, 0, MEM_RELEASE);
}
static char* os_findSymbol(const char *funcName) {
    HANDLE process = ::OpenProcess(PROCESS_QUERY_INFORMATION | PROCESS_VM_READ, FALSE, GetCurrentProcessId());
    ASSERT(process != NULL);

    vector<HMODULE> modules;
    {
        DWORD bytes;
        ::EnumProcessModules(process, NULL, 0, &bytes);
        modules.resize(bytes / sizeof(modules[0]));
        ::EnumProcessModules(process, &modules[0], bytes, &bytes);
    }
    ASSERT(modules.size() > 0);

    char *func = NULL;
    for (auto module : modules) {
        if (func = ::GetProcAddress(module, funcName)) {
            break;
        }
    }

    ::CloseHandle(process);
    return func;
}
#endif
//==============================
#if defined(__linux__) || defined(__APPLE__)
#include <dlfcn.h>
#include <unistd.h>
#include <sys/mman.h>
static char* os_mallocExecutable(int size) {
    char *p = NULL;
    int erro = ::posix_memalign((void**)&p, ::getpagesize(), size);
    ASSERT(erro == 0 && p != NULL);
    erro = ::mprotect(p, size, PROT_READ | PROT_WRITE | PROT_EXEC);
    ASSERT(erro == 0);
    return p;
}
static void os_freeExecutable(char *p) {
    ASSERT(p != NULL);
    ::free(p);
}
static char* os_findSymbol(const char *funcName) {
    void *m = ::dlopen(NULL, RTLD_NOW);
    char *r = (char*)::dlsym(m, funcName);
    ::dlclose(m);
    return r;
}
#endif
//============================== code generator
class x86FunctionBuilder;
class x86JITEngine {
public:
    x86JITEngine(): m_usedTextSectionSize(0), m_totalTextSectionSize(4096 * 8) {
        m_textSection = os_mallocExecutable(m_totalTextSectionSize);
    }
    ~x86JITEngine() { os_freeExecutable(m_textSection); }
    char* getFunction(const string &name) { return *_getFunctionEntry(name); }

    void _beginBuild();
    char** _getFunctionEntry(const string &name) { return &m_funcEntries[name]; }
    const char* _getLiteralStringLoc(const string &literalStr) { m_literalStrs.insert(literalStr).first->c_str();}
    x86FunctionBuilder* _beginBuildFunction();
    void _endBuildFunction(x86FunctionBuilder *builder);
    void _endBuild();
private:
    char *m_textSection;
    int m_usedTextSectionSize, m_totalTextSectionSize;
    map<string, char*> m_funcEntries;
    set<string> m_literalStrs;
};
class x86Label { 
public:
    x86Label(): m_address(NULL){}
    ~x86Label() { ASSERT(m_address != NULL); }
    void mark(char *address) {
        ASSERT(m_address == NULL);
        m_address = address;
        bindRefs();
    }
    void addRef(char *ref) {
        m_refs.push_back(ref);
        bindRefs();
    }
private:
    void bindRefs() {
        if (m_address == NULL) return;
        for (int i = 0; i < (int)m_refs.size(); ++i) {
            *(int*)m_refs[i] = m_address - (m_refs[i] + 4);
        }
        m_refs.clear();
    }
private:
    char *m_address;
    vector<char*> m_refs;
};
#define MAX_LOCAL_COUNT 64
class x86FunctionBuilder {
public:
    x86FunctionBuilder(x86JITEngine *parent, char *codeBuf): m_parent(parent), m_codeBuf(codeBuf), m_codeSize(0){}
    string& getFuncName() { return m_funcName;}
    int getCodeSize() const{ return m_codeSize;}

    void beginBuild(){
        // push edx
        // push ebp
        // mov ebp, esp
        // sub esp, MAX_LOCAL_COUNT * 4
    }
    void endBuild(){
        markLabel(&m_retLabel);
        // mov esp, ebp
        // pop ebp
        // pop edx
        // ret
    }

    void loadImm(int imm){
        // push imm
    }
    void loadLiteralStr(const string &literalStr){
        const char *loc = m_parent->_getLiteralStringLoc(literalStr);
        // push loc
    }
    void loadLocal(int idx){
        if (idx < 0) {
            // push dword_ptr [ebp + 4 - idx * 4]
        } else {
            // push dword_ptr [ebp - (idx + 1) * 4]
        }
    }
    void storeLocal(int idx) {
        if (idx < 0) {
            // mov dword_ptr [ebp + 4 - idx * 4], dword_ptr [esp]
        } else {
            // mov dword_ptr [ebp - (idx + 1) * 4], dword_ptr [esp]
        }
        // add esp, 4
    }
    void incLocal(int idx) {
        if (idx < 0) {
            // inc dword_ptr [ebp + 4 - idx * 4]
        } else {
            // inc dword_ptr [ebp - (idx + 1) * 4]
        }
        // push [ebp - (idx + 1) * 4]
    }
    void decLocal(int idx) {
        if (idx < 0) {
            // dec dword_ptr [ebp + 4 - idx * 4]
        } else {
            // dec dword_ptr [ebp - (idx + 1) * 4]
        }
        // push [ebp - (idx + 1) * 4]
    }
    void pop(int n){
        // add esp, n * 4
    }
    void dup(){
        // push dword_ptr [esp]
    }

    void not_() {
        // not dword_ptr [esp]
    }
    void doArithmeticOp(TokenType opType) {
        // mov eax, dword_ptr [esp+4]
        switch (opType) {
            case TT_OP_ADD: 
                // add eax, dword_ptr [esp]
                break;
            case TT_OP_SUB: 
                // sub eax, dword_ptr [esp]
                break;
            case TT_OP_MUL: 
                // imul eax, dword_ptr [esp]
                break;
            case TT_OP_DIV: 
            case TT_OP_MOD: 
                // xor edx, edx
                // idiv dword_ptr [esp]
                if (opType == TT_OP_MOD) {
                    // mov eax, edx
                }
                break;  
            default: ASSERT(0); break;
        }
        // mov dword_ptr [esp+4], eax
        // add esp, 4
    }
    void cmp(TokenType cmpType) {
        x86Label label_1, label_0, label_end;
        // eax, dword_ptr [esp+4]
        // cmp eax, dword_ptr [esp]
        // add esp, 8
        switch (cmpType) { // jl label_1
            case TT_OP_LESS: break; 
            case TT_OP_LESSEQ: break;
            case TT_OP_GREATER: break;
            case TT_OP_GREATEREQ: break;
            case TT_OP_EQUAL: break;
            case TT_OP_NEQUAL: break;
        }
        // jmp label_0
        markLabel(&label_1);
        // push 1
        jmp(&label_end);
        markLabel(&label_0);
        // push 0
        markLabel(&label_end);
    }

    void markLabel(x86Label *label){ label->mark(m_codeBuf + m_codeSize); }
    void jmp(x86Label *label) { 
        char *ref = NULL;
        // jmp 
        label->addRef(ref); 
    }
    void trueJmp(x86Label *label) {
        // mov eax, dword_ptr [esp]
        // add esp, 4
        // test eax, eax
        // jnz label
    }
    void falseJmp(x86Label *label) {
        // mov eax, dword_ptr [esp]
        // add esp, 4
        // test eax, eax
        // jz label
    }
    void ret() { jmp(&m_retLabel); }
    void retExpr() {
        // mov eax, dword_ptr [esp]
        // add esp, 4
        jmp(&m_retLabel);
    }

    int beginCall(){
    }
    void endCall(const string &funcName, int callID, int paramCount){
        char ** entry = m_parent->_getFunctionEntry(funcName);
        // call [entry]
        // add esp, paramCount * 4
    }
private:
    void emit(int n, ...) {
        va_list args;
        va_start(args, n);
        for (int i = 0; i < n; ++i) m_codeBuf[m_codeSize++] = (char)va_arg(args, int);
        va_end(args);
    }
private:
    x86JITEngine *m_parent;
    char *m_codeBuf;
    int m_codeSize;
    string m_funcName;
    x86Label m_retLabel;
};
void x86JITEngine::_beginBuild() { }
x86FunctionBuilder* x86JITEngine::_beginBuildFunction() {
    x86FunctionBuilder *r = new x86FunctionBuilder(this, m_textSection + m_usedTextSectionSize);
    r->beginBuild();
    return r;
}
void x86JITEngine::_endBuildFunction(x86FunctionBuilder *builder) {
    builder->endBuild();
    *_getFunctionEntry(builder->getFuncName()) = m_textSection + m_usedTextSectionSize;
    m_usedTextSectionSize += builder->getCodeSize();
    ASSERT(m_usedTextSectionSize <= m_totalTextSectionSize);
    delete builder;
}
void x86JITEngine::_endBuild() {
    for (map<string, char*>::iterator iter = m_funcEntries.begin(); iter != m_funcEntries.end(); ++iter) {
        if (iter->second == NULL) {
            char *f = os_findSymbol(iter->first.c_str());
            ASSERT(f != NULL);
            iter->second = f;
        }
    }
}
//============================== syntax analysis
class FunctionParser {
public:
    FunctionParser(x86FunctionBuilder *builder, Scanner *scanner): m_builder(builder), m_scanner(scanner) {}
    void parse() { _function_define(); }
private:
    void _function_define() {
        m_scanner->next(1); // type
        m_builder->getFuncName() = m_scanner->next(1).lexeme;
        ASSERT(m_scanner->next(1).tt == TT_LP);
        while (m_scanner->LA(1).tt != TT_RP) {
            string type = m_scanner->next(1).lexeme;
            declareLocal(m_scanner->next(1).lexeme, type);
            if (m_scanner->LA(1).tt == TT_COMMA) m_scanner->next(1);
        }
        ASSERT(m_scanner->next(1).tt == TT_RP);
        _block();
    }
    void _block() {
        pushScope();
        ASSERT(m_scanner->next(1).tt == TT_LBRACE);
        while (m_scanner->LA(1).tt != TT_RBRACE) _statement();
        ASSERT(m_scanner->next(1).tt == TT_RBRACE);
        popScope();
    }
    void _statement() {
        switch (m_scanner->LA(1).tt) {
            case TT_SEMICELON: break;
            case TT_CONTINUE: m_scanner->next(2); m_builder->jmp(getLastContinueLabel()); break;
            case TT_BREAK: m_scanner->next(2); m_builder->jmp(getLastBreakLabel()); break;
            case TT_RETURN: 
                m_scanner->next(1);
                if (m_scanner->LA(1).tt != TT_SEMICELON) {
                    _expr(0);
                    m_builder->retExpr();
                } else m_builder->ret();
                ASSERT(m_scanner->next(1).tt == TT_SEMICELON);
                break;
            case TT_LBRACE: _block(); break;
            case TT_IF: _if(); break;
            case TT_WHILE: _while(); break;
            case TT_FOR: _for(); break;
            case TT_TYPE_CHARPTR: case TT_TYPE_INT: _local_define_list(); break;
            case TT_TYPE_VOID: ASSERT(0); break;
            default: _expr(0); m_builder->pop(1); ASSERT(m_scanner->next(1).tt == TT_SEMICELON); break;
        }
    }
    void _if() {
        x86Label label_true, label_false, label_end;

        m_scanner->next(2);
        _expr(0);
        ASSERT(m_scanner->next(1).tt == TT_RP);
        m_builder->trueJmp(&label_true);
        m_builder->jmp(&label_false);

        m_builder->markLabel(&label_true);
        _statement();
        m_builder->jmp(&label_end);

        m_builder->markLabel(&label_false);
        if (m_scanner->LA(1).tt == TT_ELSE) {
            m_scanner->next(1);
            _statement();
        }

        m_builder->markLabel(&label_end);
    }
    void _while() {
        x86Label *label_break = pushBreakLabel(), *label_continue = pushContinueLabel();

        m_builder->markLabel(label_continue);
        m_scanner->next(2);
        _expr(0);
        ASSERT(m_scanner->next(1).tt == TT_RP);
        m_builder->falseJmp(label_break);

        _statement();
        m_builder->jmp(label_continue);
        m_builder->markLabel(label_break);

        popBreakLabel(); popContinueLabel();
    }
    void _for() {
        pushScope();
        x86Label *label_continue = pushContinueLabel(), *label_break = pushBreakLabel();
        x86Label label_loop, label_body;

        m_scanner->next(2);
        switch (m_scanner->LA(1).tt) {
            case TT_SEMICELON: break;
            case TT_TYPE_INT: case TT_TYPE_CHARPTR: _local_define_list(); break;
            default: _expr(0); m_builder->pop(1); ASSERT(m_scanner->next(1).tt == TT_SEMICELON); break;
        }

        m_builder->markLabel(&label_loop);
        if (m_scanner->LA(1).tt != TT_SEMICELON) _expr(0);
        else m_builder->loadImm(1);
        ASSERT(m_scanner->next(1).tt == TT_SEMICELON);
        m_builder->falseJmp(label_break);
        m_builder->jmp(&label_body);

        m_builder->markLabel(label_continue);
        if (m_scanner->LA(1).tt != TT_RP) {
            _expr(0); 
            m_builder->pop(1);
        }
        ASSERT(m_scanner->next(1).tt == TT_RP);
        m_builder->jmp(&label_loop);

        m_builder->markLabel(&label_body);
        _statement();
        m_builder->jmp(label_continue);

        m_builder->markLabel(label_break);
        popContinueLabel(); popBreakLabel();
        popScope();
    }
    void _local_define_list() {
        string type = m_scanner->next(1).lexeme;
        _id_or_assignment(type);
        while (m_scanner->LA(1).tt == TT_COMMA) {
            m_scanner->next(1);
            _id_or_assignment(type);
        }
        ASSERT(m_scanner->next(1).tt == TT_SEMICELON);
    }
    void _id_or_assignment(const string &type) {
        Token idToken = m_scanner->next(1);
        declareLocal(idToken.lexeme, type);
        if (m_scanner->LA(1).tt == TT_OP_ASSIGN) {
            _expr(0);
            m_builder->storeLocal(getLocal(idToken.lexeme));
        }
    }
    void _expr(int rbp) {
        if (m_scanner->LA(1).tt == TT_ID && m_scanner->LA(2).tt == TT_OP_ASSIGN) {
            Token idToken = m_scanner->next(2);
            _expr(0);
            m_builder->dup();
            m_builder->storeLocal(getLocal(idToken.lexeme));
        } else {
            _expr_nud();
            while (rbp < getOperatorLBP(m_scanner->LA(1).tt)) _expr_led();
        }
    }
    void _expr_nud() {
        Token token = m_scanner->next(1);
        switch (token.tt) {
            case TT_INT: m_builder->loadImm(atoi(token.lexeme.c_str())); break;
            case TT_STRING: m_builder->loadLiteralStr(unescape(token.lexeme)); break;
            case TT_ID: 
                if (m_scanner->LA(1).tt == TT_LP) _expr_call(token);
                else m_builder->loadLocal(getLocal(token.lexeme)); 
                break;
            case TT_LP: _expr(0); ASSERT(m_scanner->next(1).tt == TT_RP); break;
            case TT_OP_NOT: _expr(getOperatorRBP(token.tt)); m_builder->not_(); break;
            case TT_OP_INC: 
            case TT_OP_DEC: {
                    int localIdx = getLocal(m_scanner->next(1).lexeme);
                    if (token.tt == TT_OP_INC) m_builder->incLocal(localIdx);
                    else m_builder->decLocal(localIdx);
                    m_builder->loadLocal(localIdx);
                } break;
            default: ASSERT(0); break;
        }
    }
    void _expr_led() {
        Token opToken = m_scanner->next(1);
        switch (opToken.tt) {
            case TT_OP_AND: case TT_OP_OR: {
                    x86Label label_end;
                    m_builder->dup();
                    if (opToken.tt == TT_OP_AND) m_builder->falseJmp(&label_end);
                    else m_builder->trueJmp(&label_end);
                    m_builder->pop(1);
                    _expr(getOperatorRBP(opToken.tt));
                    m_builder->markLabel(&label_end);
                } break;
            case TT_OP_LESS: case TT_OP_LESSEQ: case TT_OP_GREATER: case TT_OP_GREATEREQ: case TT_OP_EQUAL: case TT_OP_NEQUAL:
                _expr(getOperatorRBP(opToken.tt));
                m_builder->cmp(opToken.tt);
                break;
            case TT_OP_ADD: case TT_OP_SUB: case TT_OP_MUL: case TT_OP_DIV: case TT_OP_MOD:
                _expr(getOperatorRBP(opToken.tt));
                m_builder->doArithmeticOp(opToken.tt);
                break;
            default: ASSERT(0); break;
        }
    }
    void _expr_call(const Token &funcToken) {
        ASSERT(m_scanner->next(1).tt == TT_LP);
        int callID = m_builder->beginCall();
        int paramCount = 0;
        while (m_scanner->LA(1).tt != TT_RP) {
            ++paramCount;
            _expr(0);
            if (m_scanner->LA(1).tt == TT_COMMA) m_scanner->next(1);
        }
        ASSERT(m_scanner->next(1).tt == TT_RP);
        m_builder->endCall(funcToken.lexeme, callID, paramCount);
    }
private:
    static int getOperatorLBP(TokenType tt) {
        switch (tt) {
            case TT_RP: case TT_COMMA: case TT_SEMICELON: return 0;
            case TT_OP_AND: case TT_OP_OR: return 10;
            case TT_OP_LESS: case TT_OP_LESSEQ: case TT_OP_GREATER: case TT_OP_GREATEREQ: case TT_OP_EQUAL: case TT_OP_NEQUAL:
                return 20;
            case TT_OP_ADD: case TT_OP_SUB: return 30;
            case TT_OP_MUL: case TT_OP_DIV: case TT_OP_MOD: return 40;
            default: ASSERT(0); return 0;
        }
    }
    static int getOperatorRBP(TokenType tt) {
        switch (tt) {
            case TT_OP_NOT: return 100;
            default: return getOperatorLBP(tt);
        }
    }
private:
    void pushScope() { m_nestedLocals.resize(m_nestedLocals.size() + 1); }
    void popScope() { m_nestedLocals.pop_back(); }
    int declareArg(const string &name, const string &type) {
        ASSERT(m_args.count(name) == 0);
        return m_args[name] = -((int)m_args.size() + 1);
    }
    int declareLocal(const string &name, const string &type) {
        ASSERT(m_nestedLocals.back().count(name) == 0);
        int idx = 0;
        for (int i = 0; i < (int)m_nestedLocals.size(); ++i) idx += (int)m_nestedLocals[i].size();
        ASSERT(idx + 1 <= MAX_LOCAL_COUNT);
        return m_nestedLocals.back()[name] = idx;
    }
    int getLocal(const string &name) {
        map<string, int>::iterator iter;
        for (int i = 0; i < (int)m_nestedLocals.size(); ++i) {
            iter = m_nestedLocals[i].find(name);
            if (iter != m_nestedLocals[i].end()) return iter->second;
        }
        iter = m_args.find(name);
        ASSERT(iter != m_args.end());
        return iter->second;
    }
    x86Label* pushContinueLabel() { m_continueLabels.push_back(new x86Label()); return m_continueLabels[0]; }
    void popContinueLabel() { delete m_continueLabels.back(); m_continueLabels.pop_back();}
    x86Label* getLastContinueLabel() { return m_continueLabels.back(); }
    x86Label* pushBreakLabel(){ m_breakLabels.push_back(new x86Label()); return m_breakLabels[0]; }
    void popBreakLabel(){ delete m_breakLabels.back(); m_breakLabels.pop_back();}
    x86Label* getLastBreakLabel() { return m_breakLabels.back(); }
private:
    x86FunctionBuilder *m_builder;
    Scanner *m_scanner;
    vector<map<string, int> > m_nestedLocals;
    map<string, int> m_args;
    vector<x86Label*> m_continueLabels, m_breakLabels;
};
class FileParser {
public:
    FileParser(x86JITEngine *engine, Scanner *scanner): m_engine(engine), m_scanner(scanner) {}
    void parse() { while (parseFunction()); }
private:
    bool parseFunction() {
        if (m_scanner->LA(1).tt != TT_EOF) {
            x86FunctionBuilder *builder = m_engine->_beginBuildFunction();
            FunctionParser(builder, m_scanner).parse();
            m_engine->_endBuildFunction(builder);
            return true;
        }
        return false;
    }
private:
    x86JITEngine *m_engine;
    Scanner *m_scanner;
};
//============================== 
static string readFile(const string &name) {
    string r;
    FILE *f = fopen(name.c_str(), "r");
    if (f == NULL) return r;
    fseek(f, 0, SEEK_END);
    int size = ftell(f);
    fseek(f, 0, SEEK_SET);
    r.resize(size + 1, 0);
    int bytes = fread(&r[0], size, 1, f);
    ASSERT(bytes == size);
    fclose(f);
    return r;
}
int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage : %s filename\n", argv[0]);
        return 1;
    }

    try {
        string source = readFile(argv[1]);
        Scanner scanner(source.c_str());

        x86JITEngine engine;
        FileParser(&engine, &scanner).parse();
        int r = ((int(*)())engine.getFunction("main"))();
        printf("ret by main : %d\n", r);
        return r;
    } catch(const char *e) {
        printf("unhandled exception: %s\n", e);
        return 1;
    }
}
