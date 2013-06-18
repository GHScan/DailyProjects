
class FunctionMeta(object):
    def __init__(self, retTypeStr, name, argTypeNamePairs, isVarArg, bodyStmt):
        self.retTypeStr, self.name, self.argTypeNamePairs, self.isVarArg, self.bodyStmt = retTypeStr, name, argTypeNamePairs, isVarArg, bodyStmt

class SourceMeta(object):
    def __init__(self):
        self.globalVars = []
        self.externFuncs = []
        self.funcs = []
