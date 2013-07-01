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

func handleBuildinCall(node *ast.CallExpr, vars map[string]interface{}) (reflect.Value, bool) {
    if funIdent, ok := node.Fun.(*ast.Ident); ok {
        switch funIdent.Name {
        case "cap":
            if len(node.Args) != 1 {
                panic(evalError("cap() requires 1 argument"))
            }
            return reflect.ValueOf(visit(node.Args[0], vars).Cap()), true
        case "len":
            if len(node.Args) != 1 {
                panic(evalError("len() requires 1 argument"))
            }
            return reflect.ValueOf(visit(node.Args[0], vars).Len()), true
        }
    }
    return reflect.Value{}, false
}

func visit(_node ast.Node, vars map[string]interface{}) reflect.Value {
    switch node := _node.(type) {
    case *ast.Ident:
        return reflect.ValueOf(vars[node.Name])
    case *ast.BasicLit:
        // FIXME: the type of literal is context senstive...
        switch node.Kind {
        case token.INT:
            if v, err := strconv.ParseInt(node.Value, 0, 32); err != nil {
                panic(err)
            } else {
                return reflect.ValueOf(int(v))
            }
        case token.FLOAT:
            if v, err := strconv.ParseFloat(node.Value, 64); err != nil {
                panic(err)
            } else {
                return reflect.ValueOf(v)
            }
        case token.CHAR:
            if v, _, _, err := strconv.UnquoteChar(node.Value[1:len(node.Value)-1], '\''); err != nil {
                panic(err)
            } else {
                return reflect.ValueOf(v)
            }
        case token.STRING:
            if v, err := strconv.Unquote(node.Value); err != nil {
                panic(err)
            } else {
                return reflect.ValueOf(v)
            }
        default:
            panic(evalError("unsupported literal type:" + fmt.Sprint(node.Kind)))
        }
    case *ast.ParenExpr:
        return visit(node.X, vars)
    case *ast.SelectorExpr:
        return visit(node.X, vars).FieldByName(node.Sel.Name)
    case *ast.IndexExpr:
        x := visit(node.X, vars)
        return x.Index(visit(node.Index, vars).Interface().(int))
    case *ast.SliceExpr:
        x := visit(node.X, vars)
        low := 0
        if node.Low != nil {
            low = visit(node.Low, vars).Interface().(int)
        }
        high := x.Len()
        if node.High != nil {
            high = visit(node.High, vars).Interface().(int)
        }
        return x.Slice(low, high)
    case *ast.CallExpr:
        if v, ok := handleBuildinCall(node, vars); ok { 
            return v
        }

        fun := visit(node.Fun, vars)
        args := []reflect.Value{}
        for _, exp := range node.Args {
            args = append(args, visit(exp, vars))
        }
        rets := fun.Call(args)
        if len(rets) == 1 {
            return rets[0]
        } else {
            panic(evalError("wrong number of ret value"))
        }
    case *ast.StarExpr:
        return visit(node.X, vars).Elem()
    case *ast.UnaryExpr:
        v := visit(node.X, vars)
        switch node.Op {
        case token.NOT:
            return reflect.ValueOf(!v.Interface().(bool));
        default:
            panic(evalError("not implemented unary operator:" + fmt.Sprint(node.Op)))
        }
    case *ast.BinaryExpr:
        // FIXME: the type of left & right value
        // FIXME: the right logic of '&&' and '||'
        x := visit(node.X, vars)
        y := visit(node.Y, vars)
        switch node.Op {
        case token.ADD:
            return reflect.ValueOf(x.Interface().(int) + y.Interface().(int))
        case token.SUB:
            return reflect.ValueOf(x.Interface().(int) - y.Interface().(int))
        case token.MUL:
            return reflect.ValueOf(x.Interface().(int) * y.Interface().(int))
        case token.QUO:
            return reflect.ValueOf(x.Interface().(int) / y.Interface().(int))
        case token.REM:
            return reflect.ValueOf(x.Interface().(int) % y.Interface().(int))
        case token.LSS:
            return reflect.ValueOf(x.Interface().(int) < y.Interface().(int))
        case token.LEQ:
            return reflect.ValueOf(x.Interface().(int) <= y.Interface().(int))
        case token.GTR:
            return reflect.ValueOf(x.Interface().(int) > y.Interface().(int))
        case token.GEQ:
            return reflect.ValueOf(x.Interface().(int) >= y.Interface().(int))
        case token.EQL:
            return reflect.ValueOf(x.Interface().(int) == y.Interface().(int))
        case token.NEQ:
            return reflect.ValueOf(x.Interface().(int) != y.Interface().(int))
        case token.LAND:
            return reflect.ValueOf(x.Interface().(bool) && y.Interface().(bool))
        case token.LOR:
            return reflect.ValueOf(x.Interface().(bool) || y.Interface().(bool))
        default:
            panic(evalError("not implemented binary operator:" + fmt.Sprint(node.Op)))
        }
    default:
        panic(evalError("not supported sytnax : " + fmt.Sprint(node)))
    }
    return reflect.Value{}
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

    return visit(node, vars).Interface(), nil
}
