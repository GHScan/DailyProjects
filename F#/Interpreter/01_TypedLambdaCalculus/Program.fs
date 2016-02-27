module Program

type Expr =
    | Cons of int
    | Var of string
    | App of Expr * Expr
    | Lambda of string * Expr

type Value = 
    | Int of int
    | Function of (int -> int)
    | Closure of string * Expr * (string * Value) list

let rec interpret (expr : Expr) (env : (string * Value) list) : Value =
    match expr with
    | Cons i -> Int i
    | Var name -> 
        match List.tryFind (fst >> (=) name) env with
        | None -> failwith (sprintf "Invalid variable %s" name)
        | Some (name, v) -> v
    | App (func, arg) -> apply (interpret func env) (interpret arg env)
    | Lambda (name, body) -> Closure(name, body, env)

and apply (func : Value) (arg : Value) : Value = 
    match (func, arg) with
    | (Function f, Int i) -> f i |> Int
    | (Closure (name, body, env), _) -> interpret body ((name, arg) :: env)
    | _ -> failwith (sprintf "Invalid apply %A %A" func arg)

[<EntryPoint>]
let main argv = 
    let G =
        [
            "Double", Function (fun i -> i * i)
        ]
    let eval e = interpret e G
    
    printfn "%A" (eval (App(Var "Double", Cons 3)))
    printfn "%A" (eval (App(Lambda("a", App (Var "Double", Var "a")), Cons 3)))

    0 