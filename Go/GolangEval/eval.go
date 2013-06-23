package main

import (
    "go/parser"
    "go/ast"
    "go/token"
    "fmt"
    "strconv"
    "reflect"
)

type evalError string
func (this evalError) Error() string {
    return string(this)
}

type exprVisitor struct {
    vars map[string]interface{}
    value reflect.Value
}
func (this *exprVisitor) handleBuildinCall(node *ast.CallExpr) bool {
    if funIdent := node.Fun.(*ast.Ident); funIdent != nil {
        switch funIdent.Name {
        case "cap":
            if len(node.Args) != 1 {
                panic(evalError("cap() requires 1 argument"))
            }
            ast.Walk(this, node.Args[0])
            this.value = reflect.ValueOf(this.value.Cap())
            return true
        case "len":
            if len(node.Args) != 1 {
                panic(evalError("len() requires 1 argument"))
            }
            ast.Walk(this, node.Args[0])
            this.value = reflect.ValueOf(this.value.Len())
            return true
        }
    }
    return false
}
func (this *exprVisitor) Visit(_node ast.Node) (w ast.Visitor) {
    switch node := _node.(type) {
    case *ast.Ident:
        this.value = reflect.ValueOf(this.vars[node.Name])
    case *ast.BasicLit:
        // FIXME: the type of literal is context senstive...
        switch node.Kind {
        case token.INT:
            if v, err := strconv.ParseInt(node.Value, 0, 32); err != nil {
                panic(err)
            } else {
                this.value = reflect.ValueOf(int(v))
            }
        case token.FLOAT:
            if v, err := strconv.ParseFloat(node.Value, 64); err != nil {
                panic(err)
            } else {
                this.value = reflect.ValueOf(v)
            }
        case token.CHAR:
            if v, _, _, err := strconv.UnquoteChar(node.Value[1:len(node.Value)-1], '\''); err != nil {
                panic(err)
            } else {
                this.value = reflect.ValueOf(v)
            }
        case token.STRING:
            if v, err := strconv.Unquote(node.Value); err != nil {
                panic(err)
            } else {
                this.value = reflect.ValueOf(v)
            }
        default:
            panic(evalError("unsupported literal type:" + fmt.Sprint(node.Kind)))
        }
    case *ast.ParenExpr:
        ast.Walk(this, node.X)
    case *ast.SelectorExpr:
        ast.Walk(this, node.X)
        this.value = this.value.FieldByName(node.Sel.Name)
    case *ast.IndexExpr:
        ast.Walk(this, node.X)
        x := this.value
        ast.Walk(this, node.Index)
        this.value = x.Index(this.value.Interface().(int))
    case *ast.SliceExpr:
        ast.Walk(this, node.X)
        x := this.value
        low := 0
        if node.Low != nil {
            ast.Walk(this, node.Low)
            low = this.value.Interface().(int)
        }
        high := x.Len()
        if node.High != nil {
            ast.Walk(this, node.High)
            high = this.value.Interface().(int)
        }
        this.value = x.Slice(low, high)
    case *ast.CallExpr:
        if this.handleBuildinCall(node) { return }

        ast.Walk(this, node.Fun)
        fun := this.value
        args := []reflect.Value{}
        for _, exp := range node.Args {
            ast.Walk(this, exp)
            args = append(args, this.value)
        }
        rets := fun.Call(args)
        if len(rets) == 0 {
            this.value = reflect.ValueOf(nil)
        } else {
            this.value = rets[0]
        }
    case *ast.StarExpr:
        ast.Walk(this, node.X)
        this.value = this.value.Elem()
    case *ast.UnaryExpr:
        ast.Walk(this, node.X)
        switch node.Op {
        case token.NOT:
            this.value = reflect.ValueOf(!this.value.Interface().(bool));
        default:
            panic(evalError("not implemented unary operator:" + fmt.Sprint(node.Op)))
        }
    case *ast.BinaryExpr:
        // FIXME: the type of left & right value
        // FIXME: the right logic of '&&' and '||'
        ast.Walk(this, node.X)
        x := this.value
        ast.Walk(this, node.Y)
        y := this.value
        switch node.Op {
        case token.ADD:
            this.value = reflect.ValueOf(x.Interface().(int) + y.Interface().(int))
        case token.SUB:
            this.value = reflect.ValueOf(x.Interface().(int) - y.Interface().(int))
        case token.MUL:
            this.value = reflect.ValueOf(x.Interface().(int) * y.Interface().(int))
        case token.QUO:
            this.value = reflect.ValueOf(x.Interface().(int) / y.Interface().(int))
        case token.REM:
            this.value = reflect.ValueOf(x.Interface().(int) % y.Interface().(int))
        case token.LSS:
            this.value = reflect.ValueOf(x.Interface().(int) < y.Interface().(int))
        case token.LEQ:
            this.value = reflect.ValueOf(x.Interface().(int) <= y.Interface().(int))
        case token.GTR:
            this.value = reflect.ValueOf(x.Interface().(int) > y.Interface().(int))
        case token.GEQ:
            this.value = reflect.ValueOf(x.Interface().(int) >= y.Interface().(int))
        case token.EQL:
            this.value = reflect.ValueOf(x.Interface().(int) == y.Interface().(int))
        case token.NEQ:
            this.value = reflect.ValueOf(x.Interface().(int) != y.Interface().(int))
        case token.LAND:
            this.value = reflect.ValueOf(x.Interface().(bool) && y.Interface().(bool))
        case token.LOR:
            this.value = reflect.ValueOf(x.Interface().(bool) || y.Interface().(bool))
        default:
            panic(evalError("not implemented binary operator:" + fmt.Sprint(node.Op)))
        }
    default:
        panic(evalError("not supported sytnax : " + fmt.Sprint(node)))
    }
    return
}

func Eval(exprStr string, vars map[string]interface{}) (ret interface{}, err error) {
    node, err := parser.ParseExpr(exprStr)
    if err != nil { return }

    defer func() { 
        if anyErr := recover(); anyErr != nil {
            if _err, ok := anyErr.(error); ok {
                err = _err
            } else {
                err = evalError(fmt.Sprint(anyErr))
            }
        }
    }()

    v := exprVisitor{vars, reflect.ValueOf(1)}
    ast.Walk(&v, node)
    return v.value.Interface(), nil
}
