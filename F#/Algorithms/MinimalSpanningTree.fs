module MinimalSpanningTree

open NUnit.Framework
open Graphs
open PriorityQueue
open DisjointSet

type LazyPrimMST(graph : UndirectedGraph) = 

    let findEdges() = 
        let pq = new PriorityQueue<float * int * int>()
        let marks = Array.create (graph.VertexCount) false
        let mutable edges = []
        
        let mutable remainingVertex = graph.VertexCount - 1
        marks.[0] <- true
        for (t, w) in graph.Adjacencies(0) do
            pq.Push(w, 0, t)

        while remainingVertex > 0 do
            match pq.Pop() with
            | w, f, t when not marks.[t] -> 
                remainingVertex <- remainingVertex - 1
                marks.[t] <- true
                edges <- (f, t, w) :: edges
                for (t2, w) in graph.Adjacencies(t) do
                    if not marks.[t2] then
                        pq.Push(w, t, t2)
            | _ -> ()

        edges

    let edges = findEdges()

    member this.Edges : list<int * int * float> = edges

    member this.TotalWeight = 
        this.Edges
        |> List.map (fun (a, b, w) -> w)
        |> List.sum

type PrimMST(graph : UndirectedGraph) =

    let findEdges() =
        let mutable edges = []
        let pq = new IndexablePriorityQueue<float * int>(0, graph.VertexCount - 1)

        for t in 1..graph.VertexCount - 1 do
            pq.Push(t, (System.Double.MaxValue, 0))
        for (t, w) in graph.Adjacencies(0) do
            pq.Remove(t) |> ignore
            pq.Push(t, (w, 0))
        while not (pq.Empty) do
            let t, (w, f) = pq.Pop()
            edges <- (f, t, w) :: edges
            for (t2, w) in graph.Adjacencies(t) do
                if pq.Contains(t2) && fst (pq.Get(t2)) > w then
                    pq.Remove(t2) |> ignore
                    pq.Push(t2, (w, t))

        edges
    
    let edges = findEdges()

    member this.Edges : list<int * int * float> = edges

    member this.TotalWeight = 
        this.Edges
        |> List.map (fun (a, b, w) -> w)
        |> List.sum

type KruskalMST(graph : UndirectedGraph) =

    let findEdges() =
        let mutable edges = []
        let ds = new DisjointSet(graph.VertexCount)
        let pq = new PriorityQueue<float * int * int>()

        for f in 0..graph.VertexCount - 1 do
            for (t, w) in graph.Adjacencies(f) do
                if f < t then pq.Push(w, f, t)

        while ds.Count > 1 do
            let w, f, t = pq.Pop()
            if not (ds.IsSame(f, t)) then
                ds.Union(f, t)
                edges <- (f, t, w) :: edges

        edges
    
    let edges = findEdges()

    member this.Edges : list<int * int * float> = edges

    member this.TotalWeight = 
        this.Edges
        |> List.map (fun (a, b, w) -> w)
        |> List.sum

[<TestFixture>]
type internal MinimalSpanningTreeTest() = 
    [<Test>]
    member this.TestWithGraphFile() = 
        let fileNames = ["TinyWUG.txt"; "MediumWUG.txt"]
        for fileName in fileNames do
            let graph = UndirectedGraph.Load (sprintf "../../Data/%s" fileName)

            let lazyPrimMst = new LazyPrimMST(graph)
            let primMst = new PrimMST(graph)
            let kruskalMst = new KruskalMST(graph)

            Assert.IsTrue(System.Math.Abs(lazyPrimMst.TotalWeight - primMst.TotalWeight) < 0.05)
            Assert.IsTrue(System.Math.Abs(lazyPrimMst.TotalWeight - kruskalMst.TotalWeight) < 0.05)

    member this.BenchWithGraphFile() = 
        let graph = UndirectedGraph.Load (sprintf "../../Data/%s" "10000WUG.txt")

        Utility.timeit "Bench MST : LazyPrim" 3 (fun ()-> new LazyPrimMST(graph))
        Utility.timeit "Bench MST : Prim" 3 (fun ()-> new PrimMST(graph))
        Utility.timeit "Bench MST : Kruskal" 3 (fun ()-> new KruskalMST(graph))