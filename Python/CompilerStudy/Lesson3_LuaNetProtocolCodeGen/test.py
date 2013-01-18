#!/usr/bin/env python

import re

TT_id = 1
TT_notation = 2
class Token(object):
    __slots__ = ['type', 'value']
    def __init__(self, t, v):
        self.type, self.value = t, v

def lexicalAnalysis(src):
    src = re.sub(r'//.*', '', src)
    tokens = []
    for m in re.finditer(r'([_a-zA-Z]\w*)|(\S)', src):
        if m.group(1):
            tokens.append(Token(TT_id, m.group(1)))
        elif m.group(2):
            tokens.append(Token(TT_notation, m.group(2)))
    return tokens

class SyntaxParser(object):
    def __init__(self, tokens):
        self.m_tokens = tokens
        self.m_curToken = 0
    def parse(self):
        structs = []
        while True:
            s = self.parseStruct()
            if not s: break
            structs.append(s)
        return structs
    def parseStruct(self):
        if not self.consumeToken(TT_id, 'struct'):
            return
        s = {'name' :self.consumeToken(TT_id), 'fields': []}
        self.consumeToken(TT_notation, '{')
        while True:
            f = self.parseField()
            if not f: break
            s['fields'].append(f)
        self.consumeToken(TT_notation, '}')
        self.consumeToken(TT_notation, ';')
        return s
    def parseField(self):
        t = self.consumeToken(TT_id)
        if not t: return 
        val = self.consumeToken(TT_id)
        self.consumeToken(TT_notation, ';')
        return [t, val]
    def consumeToken(self, t, s = None):
        if self.m_curToken >= len(self.m_tokens): return
        if self.m_tokens[self.m_curToken].type != t: return
        if s and self.m_tokens[self.m_curToken].value != s: return
        self.m_curToken += 1
        return self.m_tokens[self.m_curToken - 1].value

class LuaCodeGen(object):
    def gen(self, structs, f):
        for s in structs:
            self.genReadStruct(f, s)
            self.genWriteStruct(f, s)
    def genReadStruct(self, f, s):
        f.write('function read_%s(f)\n' % (s['name']))
        f.write('\tlocal r = {}\n')
        for field in s['fields']:
            f.write('\tr.%s = read_%s(f)\n' % (field[1], field[0]))
        f.write('\treturn r\n')
        f.write('end\n')
    def genWriteStruct(self, f, s):
        f.write('function write_%s(f, s)\n' % (s['name']))
        for field in s['fields']:
            f.write('\twrite_%s(f, s.%s)\n' % (field[0], field[1]))
        f.write('end\n')

structs = SyntaxParser(lexicalAnalysis(file('1.txt').read())).parse()
LuaCodeGen().gen(structs, file('2.lua', 'w'))
