package main
import (
    "fmt"
    "regexp"
    "math/big"
)

type Operator struct {
    op string
    hash int
}
func (this *Operator) Call(a, b interface{}) interface{} {
    switch this.op {
    case "+":
        if v, ok := a.(string); ok {
            return v + fmt.Sprint(b)
        }
        return new(big.Int).Add(a.(*big.Int), b.(*big.Int))
    case "-": return new(big.Int).Sub(a.(*big.Int), b.(*big.Int))
    case "*":
        if v, ok := a.(string); ok {
            r := v
            for i := int64(1); i < b.(*big.Int).Int64(); i++ { r += v }
            return r
        }
        return new(big.Int).Mul(a.(*big.Int), b.(*big.Int))
    case "/": return new(big.Int).Div(a.(*big.Int), b.(*big.Int))
    case "!": return new(big.Int).Exp(a.(*big.Int), b.(*big.Int), nil)
    }
    return nil
}

var reg, _ = regexp.Compile(`(\d+)|('.*')|([\+\-\*\/\!])|(\()|(\))`)
var opPriority = map[string]int { "+":1, "-":1, "*":2, "/":2, "!":3}

func _EvalTokens(tokens []interface{}) interface{} {
    if len(tokens) == 1 { return tokens[0] }
    minOpIdx, minOp := 1, tokens[1].(*Operator)
    for i, token := range tokens {
        if op, ok := token.(*Operator); ok && op.hash < minOp.hash {
            minOpIdx, minOp = i, op
        }
    }
    return minOp.Call(_EvalTokens(tokens[:minOpIdx]), _EvalTokens(tokens[minOpIdx + 1:]))
}
func Eval(expr string) interface{} {
    depth := 0
    tokens := []interface{}{}
    for i, m := range reg.FindAllStringSubmatch(expr, -1) {
        switch {
        case len(m[1]) > 0:
            b := new(big.Int)
            fmt.Sscan(m[1], b)
            tokens = append(tokens, b)
        case len(m[2]) > 0:
            tokens = append(tokens, m[2][1:len(m[2]) - 1])
        case len(m[3]) > 0:
            tokens = append(tokens, &Operator{m[3], 
                (depth << 24) | (opPriority[m[3]] << 16) | (0xffff - i)})
        case len(m[4]) > 0: depth++
        case len(m[5]) > 0: depth--
        }
    }
    return _EvalTokens(tokens)
}

func main() {
    fmt.Println(Eval("1 + (11-(2+3)) * 4"))
    fmt.Println(Eval("2!10-3"))
    fmt.Println(Eval("'abc'*3"))
    fmt.Println(Eval("'abc'+(2!3*2+1)"))
}
