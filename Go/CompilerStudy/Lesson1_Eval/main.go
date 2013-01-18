package main
import (
    "fmt"
    "regexp"
    "math/big"
)

type IValue interface {
    Add(IValue) IValue
    Sub(IValue) IValue
    Mul(IValue) IValue
    Div(IValue) IValue
    Power(IValue) IValue
}
type IntValue struct {
    num *big.Int
}
func (this IntValue) Add(v IValue) IValue {
    return IntValue{new(big.Int).Add(this.num, v.(IntValue).num)}
}
func (this IntValue) Sub(v IValue) IValue {
    return IntValue{new(big.Int).Sub(this.num, v.(IntValue).num)}
}
func (this IntValue) Mul(v IValue) IValue {
    return IntValue{new(big.Int).Mul(this.num, v.(IntValue).num)}
}
func (this IntValue) Div(v IValue) IValue {
    return IntValue{new(big.Int).Div(this.num, v.(IntValue).num)}
}
func (this IntValue) Power(v IValue) IValue {
    return IntValue{new(big.Int).Exp(this.num, v.(IntValue).num, nil)}
}
func (this IntValue) String() string {
    return this.num.String()
}
type StringValue string;
func (this StringValue) Add(v IValue) IValue {
    return this + StringValue(fmt.Sprintf("%v", v))
}
func (this StringValue) Sub(v IValue) IValue {
    panic("unsupport operation!")
}
func (this StringValue) Mul(v IValue) IValue {
    r := StringValue("")
    for i := int64(0); i < v.(IntValue).num.Int64(); i++ { r += this }
    return r
}
func (this StringValue) Div(v IValue) IValue {
    panic("unsupport operation!")
}
func (this StringValue) Power(v IValue) IValue {
    panic("unsupport operation!")
}

var opPriority = map[string]int{
    "+":1, "-":1, "*":2, "/":2, "!":3,
}

type TokenType int
const (
    TT_operator = TokenType(iota)
    TT_value
)
type Token struct {
    tt TokenType
    val IValue
}

var lexicalReg, _ = regexp.Compile(`(\d+)|('[^']*')|([\+\-\*\/\(\)!])`)
func _LexicalAnalysis(expr string) []Token {
    tokens := make([]Token, 0, 32)
    for _, m := range lexicalReg.FindAllStringSubmatch(expr, -1) {
        switch {
        case len(m[1]) > 0:
            i := new(big.Int)
            fmt.Sscan(m[1], i)
            tokens = append(tokens, Token{TT_value, IntValue{i}})
        case len(m[2]) > 0:
            tokens = append(tokens, Token{TT_value, StringValue(m[2][1:len(m[2]) - 1])})
        case len(m[3]) > 0:
            tokens = append(tokens, Token{TT_operator, StringValue(m[3])})
        default:
            panic("unsupport token : " + m[0])
        }
    }
    return tokens
}

func Eval(expr string) string {
    tokens := _LexicalAnalysis(expr)
    // infix to suffix
    rStack, opStack := make([]Token, 0, 32), make([]Token, 0, 32)
    for _, token := range tokens {
        if token.tt == TT_value { 
            rStack = append(rStack, token) 
        } else {
            switch token.val.(StringValue) {
            case "(":
                opStack = append(opStack, token)
            case ")":
                for {
                    if opStack[len(opStack) - 1].val == StringValue("(") {
                        opStack = opStack[:len(opStack) - 1]
                        break
                    }
                    rStack = append(rStack, opStack[len(opStack) - 1])
                    opStack = opStack[:len(opStack) - 1]
                }
            default:
                for len(opStack) > 0 {
                    last := opStack[len(opStack) - 1]
                    if last.val == StringValue("(") { break }
                    if opPriority[string(last.val.(StringValue))] < opPriority[string(token.val.(StringValue))] { break }
                    rStack = append(rStack, opStack[len(opStack) - 1])
                    opStack = opStack[:len(opStack) - 1]
                }
                opStack = append(opStack, token)
            }
        }
    }
    for len(opStack) > 0 {
        rStack = append(rStack, opStack[len(opStack) - 1])
        opStack = opStack[:len(opStack) - 1]
    }
    // eval
    for len(rStack) > 1 {
        for i := len(rStack) - 1; i >= 2; i-- {
            opToken, valToken1, valToken2 := rStack[i], rStack[i - 2], rStack[i - 1]
            if opToken.tt != TT_operator { continue }
            if valToken1.tt != TT_value || valToken2.tt != TT_value { continue }
            switch string(opToken.val.(StringValue)) {
            case "+":
                rStack[i - 2] = Token{TT_value, valToken1.val.Add(valToken2.val)}
            case "-":
                rStack[i - 2] = Token{TT_value, valToken1.val.Sub(valToken2.val)}
            case "*":
                rStack[i - 2] = Token{TT_value, valToken1.val.Mul(valToken2.val)}
            case "/":
                rStack[i - 2] = Token{TT_value, valToken1.val.Div(valToken2.val)}
            case "!":
                rStack[i - 2] = Token{TT_value, valToken1.val.Power(valToken2.val)}
            default:
                panic("unsupport operation")
            }
            copy(rStack[i - 1:], rStack[i + 1:])
            rStack = rStack[:len(rStack) - 2]
        }
    }
    return fmt.Sprintf("%v", rStack[0].val)
}

func main() {
    fmt.Println(Eval("2!10-(5-3)"))
}
