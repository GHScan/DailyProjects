module FlowNetwork

open Graphs
open NUnit.Framework

type FlowNetwork(g : DirectedGraph) =
    let edge2Capacity = 
        seq {
            for f in 0..g.VertexCount-1 do
                for (t, w) in g.Adjacencies(f) do
                    yield (f, t), w
        } |> Map.ofSeq

    let mutable edge2Flow = edge2Capacity |> Seq.map (fun kv -> (kv.Key, 0.0)) |> Map.ofSeq

    member this.VertexCount : int = g.VertexCount
    member this.Edges(vertexIndex : int) : list<int * int> = g.Adjacencies(vertexIndex) |> List.map (fun (t, w) -> (vertexIndex, t))
    member this.StartPoint : int = 0
    member this.EndPoint : int = this.VertexCount - 1
    member this.HasEdge(f : int, t : int) : bool = edge2Capacity.TryFind(f, t).IsSome
    member this.GetEdgeCapacity(f : int, t : int) : float = edge2Capacity.[(f, t)]
    member this.GetEdgeFlow(f : int, t : int) : float = edge2Flow.[(f, t)]
    member this.AddEdgeFlow(f : int, t : int, value : float) = 
        let key = (f, t)
        let value = value + edge2Flow.[key]
        edge2Flow <- edge2Flow.Remove(key).Add(key, value)

    member this.GetVertexFlow(vertexIndex : int) : float =
        seq { for (f, t) in this.Edges(vertexIndex) -> this.GetEdgeFlow(f, t) } |> Seq.sum

type ResidualNetwork(flowNetwork : FlowNetwork) =
    let edges =
        let groups = 
            seq {
                for f in 0..flowNetwork.VertexCount-1 do
                    for (f, t) in flowNetwork.Edges(f) do
                        yield f, t
                        yield t, f
            }
            |> Seq.groupBy fst
            |> Seq.map (fun (k, l) -> (k, l |> List.ofSeq))
            |> Map.ofSeq
        [| for i in 0..flowNetwork.VertexCount-1 -> groups.[i] |]

    member this.VertexCount = flowNetwork.VertexCount
    member this.Edges(vertexIndex : int) : list<int * int> = edges.[vertexIndex]
    member this.StartPoint : int = flowNetwork.StartPoint
    member this.EndPoint : int = flowNetwork.EndPoint
    member this.GetEdgeFlow(f : int, t : int) : float = 
        if flowNetwork.HasEdge(f, t) then
            flowNetwork.GetEdgeCapacity(f, t) - flowNetwork.GetEdgeFlow(f, t)
        elif flowNetwork.HasEdge(t, f) then
            flowNetwork.GetEdgeFlow(t, f)
        else
            failwith (sprintf "Edge %A doesn't exist" (f, t))

    member this.ConsumeEdgeFlow(f : int, t : int, value : float) =
        if flowNetwork.HasEdge(f, t) then
            flowNetwork.AddEdgeFlow(f, t, value)
        elif flowNetwork.HasEdge(t, f) then
            flowNetwork.AddEdgeFlow(t, f, -value)
        else
            failwith (sprintf "Edge %A doesn't exist" (f, t))

    member this.FindPath(startPoint : int, endPoint : int) : list<int * int> option =
        let disTable = Array.create this.VertexCount (-1, System.Int32.MaxValue)
        disTable.[startPoint] <- (-1, 0)

        let mutable processList = [startPoint]

        while not processList.IsEmpty do
            let f = processList.Head
            processList <- processList.Tail
            for (f, t) in this.Edges(f) do
                let dis = snd disTable.[f] + 1
                let flow = this.GetEdgeFlow(f, t)
                if flow > 0.0 && dis < snd disTable.[t] then
                    disTable.[t] <- (f, dis)
                    processList <- t :: processList

        if fst disTable.[endPoint] = -1 then
            None
        else
            let mutable l = List.empty
            let mutable t = endPoint
            while t <> startPoint do
                let f = fst disTable.[t]
                l <- (f, t) :: l
                t <- f
            Some l

let findMaximumFlow (flowNetwork : FlowNetwork) =
    let residualNetwork = ResidualNetwork(flowNetwork)
    let startPoint, endPoint = residualNetwork.StartPoint, residualNetwork.EndPoint

    let rec loop () = 
        match residualNetwork.FindPath(startPoint, endPoint) with
        | None -> ()
        | Some path ->
            let flow = path |> Seq.map residualNetwork.GetEdgeFlow |> Seq.min
            for (f, t) in path do
                residualNetwork.ConsumeEdgeFlow(f, t, flow)
            loop()

    loop()

[<TestFixture>]
type internal FlowNetworkTest() =
    [<Test>]
    member this.TestWithTinyFN() = 
        let graph = DirectedGraph.Load "../../Data/tinyFN.txt"
        let flowNetwork = FlowNetwork(graph)

        findMaximumFlow flowNetwork

        Assert.IsTrue(System.Math.Abs(4.0 - flowNetwork.GetVertexFlow(flowNetwork.StartPoint)) < 0.05)