
class LLVMCompiler(object):
    def __init__(self, sourceMeta):
        def _printFunc(f):
            print [(k, getattr(f, k)) for k in dir(f) if not k.startswith('_')]
        for f in sourceMeta.funcs: _printFunc(f)
        for f in sourceMeta.externFuncs: _printFunc(f)
        print sourceMeta.globalVars
    def compile(self, optimize):
        pass
    def dump(self, filename):
        pass
    def run(self):
        pass
