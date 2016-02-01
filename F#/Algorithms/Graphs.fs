module Graphs

let internal load (path : string) : int * list<int * int * float> =
    use reader = System.IO.File.OpenText(path)
    let vertexCount = int (reader.ReadLine())
    let edgeCount = int (reader.ReadLine())

    vertexCount, 
    [
        for _ in 1..edgeCount do
            match reader.ReadLine().Split() with
            | [|f; t; w|] -> yield (int f, int t, float w)
            | tokens -> failwith (sprintf "invalid tokens %A" tokens)
    ]

type UndirectedGraph(vertexCount : int, edges : seq<int * int * float>) = 
    let adjacencies = 
        let groups = 
            seq { 
                for (a, b, w) in edges do
                    yield! [ a, b, w
                             b, a, w ]
            }
            |> Seq.groupBy (fun (a, _, _) -> a)
            |> Seq.map (fun (k, l) -> 
                   (k, 
                    l
                    |> Seq.map (fun (_, b, w) -> (b, w))
                    |> List.ofSeq))
            |> Map.ofSeq
        [| 0..vertexCount - 1 |] |> Array.map (fun i -> groups.[i])
    
    member this.VertexCount = vertexCount
    member this.Adjacencies(vertexIndex) : list<int * float> = adjacencies.[vertexIndex]

    static member Load(path : string) =
        let vertexCount, edges = load path
        new UndirectedGraph(vertexCount, edges)

type DirectedGraph(vertexCount : int, edges : seq<int * int * float>) = 
    let adjacencies = 
        let groups = 
            edges
            |> Seq.groupBy (fun (a, _, _) -> a)
            |> Seq.map (fun (k, l) -> 
                   (k, 
                    l
                    |> Seq.map (fun (_, b, w) -> (b, w))
                    |> List.ofSeq))
            |> Map.ofSeq
        [| 0..vertexCount - 1 |] |> Array.map (fun i -> groups.[i])
    
    member this.VertexCount = vertexCount
    member this.Adjacencies(vertexIndex) : list<int * float> = adjacencies.[vertexIndex]

    static member Load(path : string) =
        let vertexCount, edges = load path
        new DirectedGraph(vertexCount, edges)