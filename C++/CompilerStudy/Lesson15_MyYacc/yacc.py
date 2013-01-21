
import sys, os, re, math

class Parser(object):
    def __init__(self, path):
        self.m_lines = file(path).readlines()
        fname = os.path.splitext(path)[0]
        self.m_terms = {}
        self.m_nonTerms = {}
        self.m_firstNonTerm = None

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
        if not self.m_firstNonTerm: self.m_firstNonTerm = nonTerm
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
            s = s[:len(s) - len(prec)]
            prec = prec[len('%prec'):].strip()

        l = []
        tprec = None
        for m in re.findall(r"('.')|(\S+)", s):
            if m[1] in self.m_terms:
                tprec = m[1]
            l.append(m[0] or m[1])
        if not prec: prec = tprec
        l.append(prec)
        l.append(action)
        self.m_nonTerms.setdefault(nonTerm, []).append(l)

    def genCode(self):
        nonTerms = [item for item in self.m_nonTerms.iteritems()]
        extendNonTerm = '0__' + self.m_firstNonTerm
        nonTerms.insert(0, (extendNonTerm, [[self.m_firstNonTerm, None, None]]))

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
        self.m_hfile.write('\tESS_Term_End = %d,\n' % termID)
        self.m_hfile.write('\tESS_Term_Ceil = %d,\n\n' % 
                2**math.ceil(math.log(termID - 256) / math.log(2)))
        nonTermID = 1 << 16
        self.m_hfile.write('\tESS_NonTerm_Begin = %d,\n' % nonTermID)
        for k, _ in nonTerms:
            self.m_hfile.write('\tESS_%s = %d,\n' % (k, nonTermID))
            nonTermID += 1
        self.m_hfile.write('\tESS_NonTerm_End = %d,\n' % nonTermID)
        self.m_hfile.write('\tESS_NonTerm_Ceil = %d,\n' % 
                2**math.ceil(math.log(nonTermID - (1 << 16)) / math.log(2)))
        self.m_hfile.write('};\n\n')
        # func: isTerm, isNonTerm
        self.m_hfile.write('inline bool isTerm(ESyntaxSymbol sym) { return sym >= ESS_Term_Begin && sym < ESS_Term_End; }\n')
        self.m_hfile.write('inline bool isNonTerm(ESyntaxSymbol sym) { return sym >= ESS_NonTerm_Begin && sym < ESS_NonTerm_End; }\n')
        # func: toString
        self.m_hfile.write('const char * toString(ESyntaxSymbol sym);\n')
        self.m_cfile.write('const char * toString(ESyntaxSymbol sym)\n{\n')
        self.m_cfile.write('\tstatic const char *s_termTable[] = {\n')
        for term in sortedTerm:
            self.m_cfile.write('\t\t"%s",\n' % term[0])
        self.m_cfile.write('\t};\n')
        self.m_cfile.write('\tstatic const char *s_nonTermTable[] = {\n')
        for nonTerm, _ in nonTerms:
            self.m_cfile.write('\t\t"%s",\n' % nonTerm)
        self.m_cfile.write('\t};\n')
        self.m_cfile.write('\tif (isTerm(sym)) return s_termTable[sym - ESS_Term_Begin];\n')
        self.m_cfile.write('\telse if (isNonTerm(sym)) return s_nonTermTable[sym - ESS_NonTerm_Begin];\n')
        self.m_cfile.write('\telse if (sym < ESS_Term_Begin) { static string s_s; s_s = format("\'%c\'", sym); return s_s.c_str(); }\n')
        self.m_cfile.write('\telse { ASSERT(0); return ""; }\n')
        self.m_cfile.write('}\n')
        # func: getTermPriority()
        self.m_hfile.write('int getTermPriority(ESyntaxSymbol sym);\n')
        self.m_cfile.write('int getTermPriority(ESyntaxSymbol sym)\n{\n')
        self.m_cfile.write('\tstatic int s_table[] = {\n')
        for k, prop in sortedTerm:
            self.m_cfile.write('\t\t%d,\n' % prop['prior'])
        self.m_cfile.write('\t};\n')
        self.m_cfile.write('\tASSERT(sym >= ESS_Term_Begin && sym < ESS_Term_End);\n')
        self.m_cfile.write('\treturn s_table[sym - ESS_Term_Begin];\n')
        self.m_cfile.write('}\n')
        # func: getTermAssoc
        self.m_hfile.write('char getTermAssoc(ESyntaxSymbol sym);\n')
        self.m_cfile.write('char getTermAssoc(ESyntaxSymbol sym)\n{\n')
        self.m_cfile.write('\tstatic char s_table[] = {\n')
        for k, prop in sortedTerm:
            self.m_cfile.write('\t\t\'%c\',\n' % prop['assoc'])
        self.m_cfile.write('\t};\n')
        self.m_cfile.write('\tASSERT(sym >= ESS_Term_Begin && sym < ESS_Term_End);\n')
        self.m_cfile.write('\treturn s_table[sym - ESS_Term_Begin];\n')
        self.m_cfile.write('}\n')
        # func: getNonTermProductRange
        self.m_hfile.write('void getNonTermProductRange(ESyntaxSymbol sym, int &begin, int &end);\n')
        self.m_cfile.write('void getNonTermProductRange(ESyntaxSymbol sym, int &begin, int &end)\n{\n')
        self.m_cfile.write('\tstatic int s_table[][2] = {\n')
        productID2Data = dict()
        productID2Head = dict()
        productID = 0
        productBodyMaxLen = 0
        for nonTerm, prodcts in nonTerms:
            for i, v in enumerate(prodcts):
                productID2Data[productID + i] = v
                productID2Head[productID + i] = nonTerm
                productBodyMaxLen = max(len(v) - 2, productBodyMaxLen)
            newProductID = productID + len(prodcts)
            self.m_cfile.write('\t\t{%d, %d},\n' % (productID, newProductID))
            productID = newProductID
        self.m_cfile.write('\t};\n')
        self.m_cfile.write('\tASSERT(isNonTerm(sym));\n')
        self.m_cfile.write('\tbegin = s_table[sym - ESS_NonTerm_Begin][0];\n')
        self.m_cfile.write('\tend = s_table[sym - ESS_NonTerm_Begin][1];\n')
        self.m_cfile.write('}\n')
        self.m_cfile.write('#define PRODUCT_ID_END %d\n' % productID)
        self.m_cfile.write('#define PRODUCT_BODY_MAX_LEN %d\n' % productBodyMaxLen)
        # func: getProductHead
        self.m_hfile.write('ESyntaxSymbol getProductHead(int productID);\n')
        self.m_cfile.write('ESyntaxSymbol getProductHead(int productID)\n{\n')
        self.m_cfile.write('\tstatic ESyntaxSymbol s_table[PRODUCT_ID_END] = {\n');
        for pid in range(productID):
            self.m_cfile.write('\t\tESS_%s,\n' % productID2Head[pid])
        self.m_cfile.write('\t};\n')
        self.m_cfile.write('\tASSERT(productID >= 0 && productID < PRODUCT_ID_END);\n')
        self.m_cfile.write('\treturn s_table[productID];\n')
        self.m_cfile.write('}\n')
        # func: getProductConflictToken
        self.m_hfile.write('ESyntaxSymbol getProductConflictToken(int productID);\n')
        self.m_cfile.write('ESyntaxSymbol getProductConflictToken(int productID)\n{\n')
        self.m_cfile.write('\tstatic ESyntaxSymbol s_table[] = {\n')
        for pid, v in productID2Data.iteritems():
            if v[-2]:
                self.m_cfile.write('\t\tESS_%s,\n' % v[-2])
            else:
                self.m_cfile.write('\t\t(ESyntaxSymbol)-1,\n')
        self.m_cfile.write('\t};\n')
        self.m_cfile.write('\tASSERT(productID >= 0 && productID < PRODUCT_ID_END);\n')
        self.m_cfile.write('\treturn s_table[productID];\n')
        self.m_cfile.write('}\n')
        # func: getProductBody
        self.m_hfile.write('const vector<int>& getProductBody(int productID);\n')
        self.m_cfile.write('const vector<int>& getProductBody(int productID)\n{\n')
        self.m_cfile.write('\tstatic vector<vector<int> > s_table = (VectorBuilder<vector<int> >()\n')
        for pid, v in productID2Data.iteritems():
            if len(v) > 2:
                self.m_cfile.write('\t\t<< (VectorBuilder<int>() ')
                for sym in v[:-2]:
                    if sym.startswith('\''):
                        self.m_cfile.write(' << %s' % sym)
                    elif sym in self.m_terms:
                        self.m_cfile.write(' << ESS_%s' % sym)
                    else:
                        self.m_cfile.write(' << ESS_%s' % sym)
                if productID2Head[pid] == extendNonTerm:
                    self.m_cfile.write(' << ESS_Term_End')
                self.m_cfile.write(').vec // ESS_%s\n' % productID2Head[pid])
            else:
                self.m_cfile.write('\t\t<< (VectorBuilder()).vec\n')
        self.m_cfile.write('\t).vec;\n')
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
