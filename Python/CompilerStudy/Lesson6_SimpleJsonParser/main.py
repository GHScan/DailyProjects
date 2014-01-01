# vim:fileencoding=gbk

def tokenize(stream):
    import re
    tokens = []
    for m in re.finditer(r'(true|false)|(null)|((\d+)?\.\d+)|(\d+)|("[^"]*")|(\s+)|(.)', stream):
        if m.group(1): tokens.append(('BOOLEAN', m.group(1) == 'true'))
        elif m.group(2): tokens.append(('NULL', None))
        elif m.group(3): tokens.append(('FLOAT', float(m.group(3))))
        elif m.group(5): tokens.append(('INT', int(m.group(5))))
        elif m.group(6): tokens.append(('STRING', m.group(6)[1:-1]))
        elif m.group(7): continue
        elif m.group(8): tokens.append((m.group(8), None))
        else: assert False, m.group(0)
    return tokens

def _consume(tokens, tt):
    if tt and tokens[0][0] != tt: raise 'invalid json!'
    r = tokens[0]
    tokens.pop(0)
    return r

def _array(tokens):
    if tokens[0][0] != '[': return None
    _consume(tokens, '[')
    a = []
    while tokens[0][0] != ']':
        if a: _consume(tokens, ',')
        a.append(_object(tokens))
    _consume(tokens, ']')
    return a

def _dict(tokens):
    if tokens[0][0] != '{': return None
    _consume(tokens, '{')
    o = {}
    while tokens[0][0] != '}':
        if o: _consume(tokens, ',')
        key = _consume(tokens, 'STRING')[1]
        _consume(tokens, ':')
        o[key] = _object(tokens)
    _consume(tokens, '}')
    return o

def _object(tokens):
    if tokens[0][0] == 'NULL': return _consume(tokens, 'NULL')[1]
    elif tokens[0][0] == 'BOOLEAN': return _consume(tokens, 'BOOLEAN')[1]
    elif tokens[0][0] == 'INT': return _consume(tokens, 'INT')[1]
    elif tokens[0][0] == 'FLOAT': return _consume(tokens, 'FLOAT')[1]
    elif tokens[0][0] == 'STRING': return _consume(tokens, 'STRING')[1]
    o = _array(tokens)
    if o: return o
    o = _dict(tokens)
    if o: return o
    assert False, 'parse json failed!'

def parse(tokens):
    return _dict(tokens)

jsonStr = r'{"male":true, "address":{"chengdu":true, "beijing":false}, "name":"scan", "tools":[1, 2, 3.14, null, 5]}'
ast = parse(tokenize(jsonStr))
print ast
print ast['tools'][2]
print ast['address']['chengdu']
