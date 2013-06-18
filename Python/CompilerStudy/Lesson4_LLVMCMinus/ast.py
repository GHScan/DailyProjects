
def VisitorMetaClass(cname, bases, attrs):
    dispatcher = {}
    for k, v in attrs.iteritems():
        if k.startswith('visit_'):
            dispatcher[k[6:]] = v
    attrs['__dispatcher'] = dispatcher

    def _visit(self, node):
        return type(self).__dispatcher[type(node).__name__](self, node)
    attrs['visit'] = _visit

    return type(cname, bases, attrs)

class ExprNodeStringLiteral(object):
    def __init__(self, string):
        assert type(string) == str
        self.string = string
class ExprNodeIntLiteral(object):
    def __init__(self, number):
        assert type(number) == int
        self.number = number
class ExprNodeFloatLiteral(object):
    def __init__(self, number):
        assert type(number) == float
        self.number = number
class ExprNodeVariable(object):
    def __init__(self, name):
        self.name = name
class ExprNodeAssignment(object):
    def __init__(self, name, rightExpr):
        self.name, self.rightExpr = name, rightExpr
class ExprNodeBinaryOp(object):
    def __init__(self, op, leftExpr, rightExpr):
        self.op, self.leftExpr, self.rightExpr = op, leftExpr, rightExpr
class ExprNodeUnaryOp(object):
    def __init__(self, op, expr):
        self.op, self.expr = op, expr
class ExprNodeTypeCast(object):
    def __init__(self, destType, expr):
        assert type(destType) == str
        self.destType, self.expr = destType, expr
class ExprNodeCall(object):
    def __init__(self, funcName, argExprs):
        self.funcName, self.argExprs = funcName, argExprs


class StmtNodeBlock(object):
    def __init__(self, stmts):
        self.stmts = stmts
class StmtNodeStmts(object):
    def __init__(self, stmts):
        self.stmts = stmts
class StmtNodeExpr(object):
    def __init__(self, expr):
        self.expr = expr
class StmtNodeDefineVariable(object):
    def __init__(self, typeStr, name):
        self.typeStr, self.name = typeStr, name
class StmtNodeContinue(object):
    pass
class StmtNodeBreak(object):
    pass
class StmtNodeReturn(object):
    def __init__(self, expr):
        self.expr = expr
class StmtNodeIfThenElse(object):
    def __init__(self, condExpr, thenStmt, elseStmt):
        self.condExpr, self.thenStmt, self.elseStmt = condExpr, thenStmt, elseStmt
class StmtNodeFor(object):
    def __init__(self, firstStmt, secondExpr, thirdExpr, bodyStmt):
        self.firstStmt, self.secondExpr, self.thirdExpr, self.bodyStmt = firstStmt, secondExpr, thirdExpr, bodyStmt
