package main
import (
    "fmt"
    "os"
    "bufio"
)

type Address struct {
    City string
    Street string
}
type People struct {
    Name string
    Age int
    sex string
    Addr []Address
}

func add(a int, b int) *int {
    r := new(int)
    *r = a + b
    return r
}

func main() {
    vars := map[string]interface{}{
        "numArray":[]int{3, 5, 6},
        "idx": 3,
        "func_add": add,
        "peoples":[]People{
            People{"wang", 15, "male", []Address{Address{"chengdu", "xinan"}, },},
            People{"li", 27, "female", []Address{Address{"chengdu", "erhuan"}, Address{"beijing", "hh"},},},
        },
    }

    fmt.Printf("vars -> %#v\n", vars)

    stdin := bufio.NewReader(os.Stdin)
    for {
        fmt.Print("> ")
        line, err := stdin.ReadString('\n')
        if err != nil { break }

        v, err := Eval(line, vars)
        if err != nil {
            fmt.Printf("Eval error : %#v\n", err.Error())
            continue
        }

        fmt.Printf("%#v\n", v)
    }
}
