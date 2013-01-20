
import sys, os, re

class Parser(object):
    def __init__(self, path):
        self.m_lines = file(path).readlines()
        fname = os.path.splitext(path)[0]
        self.m_terms = {}
        self.m_nonTerms = {}

        self.openOFiles(*[fname + ext for ext in ['.yy.h', '.yy.cpp']])
        self.parseDefines()
        self.parseProductions()
        self.genCode()
        self.closeOFiles()

    def openOFiles(self, hfname, cfname):
        self.m_hfile = file(hfname, 'w')
        macro = hfname.replace('.', '_').upper()
        self.m_hfile.write('#ifndef %s\n' % macro)
        self.m_hfile.write('#define %s\n\n' % macro)

        self.m_cfile = file(cfname, 'w')
        self.m_cfile.write('#include "pch.h"\n')
        self.m_cfile.write('#include "%s"' % (hfname))
        
        fnameImporter = 'CodeGen.h'
        s = set(file(fnameImporter)) if os.path.isfile(fnameImporter) else set()
        s.add('#include "%s"\n' % hfname)
        file(fnameImporter, 'w').write(''.join(s))
    def closeOFiles(self):
        self.m_hfile.write('\n#endif\n')

    def parseDefines(self):
        i = 0
        prior = 1
        while i < len(self.m_lines):
            line = self.m_lines[i].strip()
            if not len(line):
                pass
            elif line == '%%':
                break
            elif line == '%{':
                while True:
                    i += 1
                    line = self.m_lines[i].strip()
                    if line == '%}':
                        break
                    self.m_hfile.write(self.m_lines[i])
            else:
                strs = line.split(' ')
                if strs[0] == '%token':
                    for k in strs[1:]:
                        self.m_terms[k] = {'assoc':'l', 'prior':0}
                elif strs[0] == '%left':
                    for k in strs[1:]:
                        self.m_terms[k] = {'assoc':'l', 'prior':prior}
                    prior += 1
                elif strs[0] == '%right':
                    for k in strs[1:]:
                        self.m_terms[k] = {'assoc':'r', 'prior':prior}
                    prior += 1
                elif strs[0] == '%nonassoc':
                    for k in strs[1:]:
                        self.m_terms[k] = {'assoc':'l', 'prior':prior}
                    prior += 1
                else:
                    assert False
            i += 1
        self.m_lines = self.m_lines[i + 1:]

    def parseProductions(self):
        i = 0
        while i < len(self.m_lines):
            line = self.m_lines[i].strip()
            if not len(line):
                pass
            elif line == '%%':
                break
            else:
                s = ''
                while True:
                    s += self.m_lines[i]
                    line = self.m_lines[i].strip()
                    if line[-1] == ';':
                        if not re.search(r"[^']{", s) or re.search(r"[^']}\s*;", s):
                            break
                    i += 1
                self.parseNonTerm(s)
            i += 1
        self.m_lines = self.m_lines[i + 1:]

        self.m_cfile.write(''.join(self.m_lines))

    def parseNonTerm(self, s):
        nonTerm = re.search('^.*?:', s).group(0)
        s = s[len(nonTerm):]
        nonTerm = nonTerm[:-1].strip()
        while len(s):
            sub = re.search(r"^(([^'{]|'.')+?|.+?[^']\})\s*[;|]", s, re.DOTALL).group(0)
            s = s[len(sub):].strip()
            self.parseProduction(nonTerm, sub[:-1])

    def parseProduction(self, nonTerm, s):
        action = re.search(r"{[^'].*$", s, re.DOTALL)
        action = action.group(0) if action else None
        if action:
            s = s[:len(s) - len(action)]
        prec = re.search(r'%prec.+$', s)
        prec = prec.group(0) if prec else None
        if prec:
            prec = prec[len('%prec'):].strip()

        l = []
        for m in re.findall(r"('.')|(\S+)", s):
            if not prec and m[1] in self.m_terms:
                prec = m[1]
            l.append(m[0] or m[1])
        l.append(self.m_terms[prec]['prior'] if prec else 0)
        l.append(action)
        self.m_nonTerms.setdefault(nonTerm, []).append(l)

    def genCode(self):
        def _cmpTerm(a, b):
            pa, pb = a[1]['prior'], b[1]['prior']
            if pa == pb:
                return cmp(a[0], b[0])
            return cmp(pa, pb)
        sortedTerm = sorted(self.m_terms.iteritems(), _cmpTerm)
        termID = 256

        # enum ESyntaxSymbol {};
        self.m_hfile.write('enum ESyntaxSymbol\n{\n\tESS_Term_Begin = %d,\n' % termID)
        for k, prop in sortedTerm:
            self.m_hfile.write('\tESS_%s = %d,\n' % (k, termID))
            termID += 1
        self.m_hfile.write('\tESS_Term_End = %d,\n\n' % termID)
        nonTermID = 1 << 16
        self.m_hfile.write('\tESS_NonTerm_Begin = %d,\n' % nonTermID)
        for k in self.m_nonTerms:
            self.m_hfile.write('\tESS_%s = %d,\n' % (k, nonTermID))
            nonTermID += 1
        self.m_hfile.write('\tESS_NonTerm_End = %d,\n' % nonTermID)
        self.m_hfile.write('};\n\n')
        # func: isTerm, isNonTerm
        self.m_hfile.write('inline bool isTerm(ESyntaxSymbol unit) { return unit >= ESS_Term_Begin && unit < ESS_Term_End; }\n')
        self.m_hfile.write('inline bool isNonTerm(ESyntaxSymbol unit) { return unit >= ESS_NonTerm_Begin && unit < ESS_NonTerm_End; }\n')
        # func: toString
        self.m_hfile.write('const char * toString(ESyntaxSymbol unit);\n')
        self.m_cfile.write('const char * toString(ESyntaxSymbol unit)\n{\n')
        self.m_cfile.write('\tstatic const char *s_termTable[] = {\n')
        for term in sortedTerm:
            self.m_cfile.write('\t\t"%s",\n' % term[0])
        self.m_cfile.write('\t};\n')
        self.m_cfile.write('\tstatic const char *s_nonTermTable[] = {\n')
        for nonTerm in self.m_nonTerms:
            self.m_cfile.write('\t\t"%s",\n' % nonTerm)
        self.m_cfile.write('\t};\n')
        self.m_cfile.write('\tif (isTerm(unit)) return s_termTable[unit - ESS_Term_Begin];\n')
        self.m_cfile.write('\telse if (isNonTerm(unit)) return s_nonTermTable[unit - ESS_NonTerm_Begin];\n')
        self.m_cfile.write('\telse if (unit < ESS_Term_Begin) { static string s_s; s_s = format("\'%c\'", unit); return s_s.c_str(); }\n')
        self.m_cfile.write('\telse { ASSERT(0); return ""; }\n')
        self.m_cfile.write('}\n')
        # func: getNonTermProductRange
        self.m_hfile.write('void getNonTermProductRange(ESyntaxSymbol unit, int &begin, int &end);\n')
        self.m_cfile.write('void getNonTermProductRange(ESyntaxSymbol unit, int &begin, int &end)\n{\n')
        self.m_cfile.write('\tstatic int s_table[][2] = {\n')
        productID2Data = dict()
        productID2Head = dict()
        productID = 0
        productBodyMaxLen = 0
        for nonTerm, prodcts in self.m_nonTerms.iteritems():
            for i, v in enumerate(prodcts):
                productID2Data[productID + i] = v
                productID2Head[productID + i] = nonTerm
                productBodyMaxLen = max(len(v) - 2, productBodyMaxLen)
            newProductID = productID + len(prodcts)
            self.m_cfile.write('\t\t{%d, %d},\n' % (productID, newProductID))
            productID = newProductID
        self.m_cfile.write('\t};\n')
        self.m_cfile.write('\tASSERT(isNonTerm(unit));\n')
        self.m_cfile.write('\tbegin = s_table[unit - ESS_NonTerm_Begin][0];\n')
        self.m_cfile.write('\tend = s_table[unit - ESS_NonTerm_Begin][1];\n')
        self.m_cfile.write('}\n')
        self.m_cfile.write('#define PRODUCT_ID_END %d\n' % productID)
        self.m_cfile.write('#define PRODUCT_BODY_MAX_LEN %d\n' % productBodyMaxLen)
        # func: getProductPriority
        self.m_hfile.write('int getProductPriority(int productID);\n')
        self.m_cfile.write('int getProductPriority(int productID)\n{\n')
        self.m_cfile.write('\tstatic int s_table[] = {\n')
        for pid, v in productID2Data.iteritems():
            self.m_cfile.write('\t\t%d,\n' % v[-2])
        self.m_cfile.write('\t};\n')
        self.m_cfile.write('\tASSERT(productID >= 0 && productID < PRODUCT_ID_END);\n')
        self.m_cfile.write('\treturn s_table[productID];\n')
        self.m_cfile.write('}\n')
        # func: getProductBody
        self.m_hfile.write('int* getProductBody(int productID);\n')
        self.m_cfile.write('int* getProductBody(int productID)\n{\n')
        self.m_cfile.write('\tstatic int s_table[][PRODUCT_BODY_MAX_LEN] = {\n')
        for pid, v in productID2Data.iteritems():
            if len(v) > 2:
                self.m_cfile.write('\t\t{ ')
                for sym in v[:-2]:
                    if sym.startswith('\''):
                        self.m_cfile.write('%s, ' % sym)
                    elif sym in self.m_terms:
                        self.m_cfile.write('ESS_%s, ' % sym)
                    else:
                        self.m_cfile.write('ESS_%s, ' % sym)
                self.m_cfile.write('}, // ESS_%s\n' % productID2Head[pid])
            else:
                self.m_cfile.write('\t\t{ 0, },\n')
        self.m_cfile.write('\t};\n')
        self.m_cfile.write('\tASSERT(productID >= 0 && productID < PRODUCT_ID_END);\n')
        self.m_cfile.write('\treturn s_table[productID];\n')
        self.m_cfile.write('}\n')
        # func: getProductAction
        for pid, v in productID2Data.iteritems():
            if not v[-1]: continue
            self.m_cfile.write('static void productionAction_%d_%s()\n{\n' % (pid, productID2Head[pid]))
            self.m_cfile.write(v[-1])
            self.m_cfile.write('\n}\n')
        self.m_cfile.write('static void productionAction_default(){}\n')
        self.m_hfile.write('void (*getProductionAction(int productID))();\n')
        self.m_cfile.write('void (*getProductionAction(int productID))()\n{\n')
        self.m_cfile.write('\tstatic void (*s_table[])() = {\n')
        for pid, v in productID2Data.iteritems():
            if not v[-1]: 
                self.m_cfile.write('\t\t&productionAction_default,\n')
            else:
                self.m_cfile.write('\t\t&productionAction_%d_%s,\n' % (pid, productID2Head[pid]))
        self.m_cfile.write('\t};\n')
        self.m_cfile.write('\tASSERT(productID >= 0 && productID < PRODUCT_ID_END);\n')
        self.m_cfile.write('\treturn s_table[productID];\n')
        self.m_cfile.write('}\n')

if len(sys.argv) == 1:
    print 'Usage : %s file' % (sys.argv[0])
else:
    Parser(sys.argv[1])
