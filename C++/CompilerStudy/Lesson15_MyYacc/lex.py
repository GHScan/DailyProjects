
import sys, os, re

class Parser(object):
    def __init__(self, fname):
        self.m_lines = file(fname).readlines()
        self.m_fname = os.path.splitext(fname)[0]
        self.m_rules = dict()
        self.parseDefines()
        self.parseActions()
    def parseDefines(self):
        ofname = self.m_fname + '.yy.h' 
        fo = file(ofname, 'w')
        macro = ofname.replace('.', '_').upper()
        fo.write('#ifndef %s\n' % macro)
        fo.write('#define %s\n\n' % macro)

        i = 0
        while i < len(self.m_lines):
            line = self.m_lines[i].strip()
            if not len(line):
                pass
            elif line == '%{':
                while True:
                    i += 1
                    line = self.m_lines[i].strip()
                    if line == '%}':
                        break
                    fo.write(self.m_lines[i])
            elif line == '%%':
                break
            else:
                name, reg = line.split(' ', 2)
                self.m_rules[name] = self._translateReg(reg)
            i += 1
        self.m_lines = self.m_lines[i + 1:]
    
        fo.write('extern map<string, int(*)()> g_reg2Action;\n')
        fo.write('extern vector<string> g_regs;\n')

        fo.write('\n#endif\n')

        fnameImporter = 'CodeGen.h'
        s = set(file(fnameImporter)) if os.path.isfile(fnameImporter) else set()
        s.add('#include "%s"\n' % ofname)
        file(fnameImporter, 'w').write(''.join(s))
    def parseActions(self):
        ofname = self.m_fname + '.yy.cpp'
        fo = file(ofname, 'w')
        fo.write('#include "pch.h"\n')
        fo.write('#include "%s"\n' % (self.m_fname + '.yy.h'))

        reg2actions = dict()
        regs = list()
        i = 0
        while i < len(self.m_lines):
            line = self.m_lines[i].strip()
            if not len(line):
                pass
            elif line == '%%':
                break
            else:
                reg, action = line.split(' ', 2)
                reg = self._translateReg(reg)
                s = action + '\n'
                while re.subn('{', '{', s)[1] != re.subn('}', '}', s)[1]:
                    i += 1
                    s += self.m_lines[i]
                reg2actions[reg] = s
                regs.append(reg)
            i += 1
        self.m_lines = self.m_lines[i + 1:]

        # g_reg2Action
        for num, regAction in enumerate(reg2actions.iteritems()):
            fo.write('static int lexAction_%d() {\n %s return 0;\n }\n' % (num, regAction[1]))
        fo.write('static map<string, int(*)()> initReg2Action(){\n')
        fo.write('\tmap<string, int(*)()> rules;\n')
        for num, regAction in enumerate(reg2actions.iteritems()):
            fo.write('\trules["%s"] = &lexAction_%d;\n' % (repr(regAction[0])[1:-1], num))
        fo.write('\treturn rules;\n }\n')
        fo.write('map<string, int(*)()> g_reg2Action = initReg2Action();\n')
        # g_regs
        fo.write('static vector<string> initRegs()\n{\n')
        fo.write('\tvector<string> regs;\n')
        for reg in regs:
            fo.write('\tregs.push_back("%s");\n' % (repr(reg)[1:-1]))
        fo.write('\treturn regs;\n')
        fo.write('}\n')
        fo.write('vector<string> g_regs = initRegs();\n')

        fo.write(''.join(self.m_lines))
    def _translateReg(self, reg):
        reg = re.sub('%', '%%', reg)
        reg = re.sub(r'{(.*?[^\\])}', r'%(\1)s', reg) 
        reg = reg % self.m_rules
        def _special(c):
            return c in r'.\[]{}()|?+*-'
        def _quote(s):
            return ''.join((r'\%s' % c if _special(c) else c) for c in s.group(1))
        reg = re.sub(r'"(.*?)"', _quote, reg)
        return reg

if len(sys.argv) == 1:
    print 'Usage : %s file' % (sys.argv[0])
else:
    Parser(sys.argv[1])
