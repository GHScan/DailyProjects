package main
import (
    "fmt"
    "regexp"
    "math/big"
    "sort"
)

type Operator struct {
    op string
    hash int
    lval, rval int
}
func (this *Operator) Call(a, b interface{}) interface{} {
    switch this.op {
    case "+": 
        if v, ok := a.(string); ok {
            return v + fmt.Sprint(b)
        } else {
            return new(big.Int).Add(a.(*big.Int), b.(*big.Int))
        }
    case "-": return new(big.Int).Sub(a.(*big.Int), b.(*big.Int))
    case "*":
        if v, ok := a.(string); ok {
            r := v
            for i := int64(1); i < b.(*big.Int).Int64(); i++ { r += v }
            return r
        } else {
            return new(big.Int).Mul(a.(*big.Int), b.(*big.Int))
        }
    case "/": return new(big.Int).Div(a.(*big.Int), b.(*big.Int))
    case "!": return new(big.Int).Exp(a.(*big.Int), b.(*big.Int), nil)
    }
    return nil
}

type OperatorList []*Operator
func (this *OperatorList) Len() int {
    return len(*this)
}
func (this *OperatorList) Less(i, j int) bool {
    return (*this)[i].hash > (*this)[j].hash
}
func (this *OperatorList) Swap(i, j int) {
    t := (*this)[i]
    (*this)[i], (*this)[j] = (*this)[j], t
}

type ValueList struct {
    vid2sid map[int]int
    sid2value map[int] interface{}
    curVid int
}
func NewValueList() *ValueList {
    return &ValueList{map[int]int{}, map[int]interface{}{}, 0}
}
func (this *ValueList) GetResult() interface{} {
    return this.Get(this.curVid)
}
func (this *ValueList) Append(v interface{}) {
    this.curVid++
    this.vid2sid[this.curVid] = this.curVid
    this.sid2value[this.curVid] = v
}
func (this *ValueList) GetNextVid() int { return this.curVid + 1 }
func (this *ValueList) GetLastVid() int { return this.curVid }
func (this *ValueList) Get(vid int) interface{} {
    this._CompressPath(vid)
    return this.sid2value[this.vid2sid[vid]]
}
func (this *ValueList) Set(vid int, v interface{}) {
    this._CompressPath(vid)
    this.sid2value[this.vid2sid[vid]] = v
}
func (this *ValueList) Assign(vid1, vid2 int) {
    this.vid2sid[vid1] = vid2
}
func (this *ValueList) _CompressPath(vid int) {
    sid := this.vid2sid[vid]
    for sid != this.vid2sid[sid] { sid = this.vid2sid[sid] }
    this.vid2sid[vid] = sid
}

var opPriority = map[string]int{ "+":1, "-":1, "*":2, "/":2, "!":3}
var re, _ = regexp.Compile(`(\d+)|('.*')|([\+\-\*\/\!])|(\()|(\))`)

func eval(expr string) interface{} {
    depth := 0
    operators, opid := OperatorList{}, 0
    values := NewValueList()
    for _, m := range re.FindAllStringSubmatch(expr, -1) {
        switch {
        case len(m[1]) > 0:
            b := new(big.Int)
            fmt.Sscan(m[1], b)
            values.Append(b)
        case len(m[2]) > 0:
            values.Append(m[2][1:len(m[2]) - 1])
        case len(m[3]) > 0:
            opid++
            hash := (depth << 24) | (opPriority[m[3]] << 16) | (0xffff - opid)
            operators = append(operators, &Operator{m[3], hash, values.GetLastVid(), values.GetNextVid()})
        case len(m[4]) > 0: depth++
        case len(m[5]) > 0: depth--
        }
    }
    sort.Sort(&operators)
    for _, op := range operators {
        values.Set(op.lval,
            op.Call(values.Get(op.lval), values.Get(op.rval)))
        values.Assign(op.rval, op.lval)
    }
    return values.GetResult()
}

func main() {
    fmt.Println(eval("1+2-3+4-5+6-7"))
    fmt.Println(eval("2*(2*(5+2) + 1) / 3"))
    fmt.Println(eval("'abc' * 3"))
    fmt.Println(eval("'abc' + (3 * 5)"))
    fmt.Println(eval("2!10-1"))
}
